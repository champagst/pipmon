from gitlab import Gitlab
from gitlab.exceptions import GitlabGetError
from pipmon.rpc import JSONRPCServer, Fault
from netrc import netrc
from time import time
from tempfile import TemporaryFile
import os
import requests
import yaml

from pprint import pprint

TRACE_CHUNK_SIZE = int(os.getenv("PIPMON_TRACE_CHUNK_SIZE", 10))
REQUEST_TIMEOUT = int(os.getenv("PIPMON_REQUEST_TIMEOUT", 5000))
TRAIL_BY = int(os.getenv("PIPMON_TRAIL_BY", 5))

class Trace:
    def __init__(self, job):
        self.page = 0
        self.finished = False
        self.paused = False
        self.ts = 0
        self.buffer = TemporaryFile()
        self.length = 0
        self.offset = 0
        self.job = job

def into_pipeline(data):
    return {
        "source": "pipeline",
        "payload": {
            "project_id": data.project_id,
            "id": data.id,
            "status": data.detailed_status["text"],
            "updated_at": data.updated_at,
            "finished_at": data.finished_at,
            "name": data.attributes.get("name"),
            "web_url": data.web_url,
        }
    }

def into_job(data):
    return {
        "source": "job",
        "payload": {
            "project_id": data.pipeline["project_id"],
            "pipeline_id": data.pipeline["id"],
            "job_id": data.id,
            "name": data.name,
            "status": data.status,
            "finished_at": data.finished_at,
        }
    }

def time_ms():
    return round(time() * 1000)
    
class PipmonServer(JSONRPCServer):
    def __init__(self, *args, **kwargs):
        super(PipmonServer, self).__init__(*args, **kwargs)

        with open(os.path.expanduser("~/.pipmon.yml"), "r") as f:
            self._conf = yaml.safe_load(f)
            
        self._netrc = netrc(os.path.expanduser("~/.authinfo"))
        self._gl = Gitlab(self._conf["url"], private_token=self._netrc.authenticators(self._conf["private_token_key"])[2])
        self._session = requests.Session()
        self._requests = []
        self._trace = None
        self._request_ts = time_ms()

    def _run_pipeline(self, project_id, pipeline_id):
        project = self._gl.projects.get(project_id, lazy=True)
        pipeline = project.pipelines.get(pipeline_id)
        return into_pipeline(pipeline)

    def _run_jobs(self, project_id, pipeline_id):
        project = self._gl.projects.get(project_id, lazy=True)
        pipeline = project.pipelines.get(pipeline_id)
        jobs = pipeline.jobs.list()
        return [into_job(job) for job in jobs]
        
    def _run_job(self, project_id, job_id):
        project = self._gl.projects.get(project_id)
        job = project.jobs.get(job_id)
        return into_job(job)

    def _next_page(self):
        range_start = TRACE_CHUNK_SIZE * self._trace.page
        if self._trace.finished and range_start > self._trace.length:
            return
        self._trace.buffer.seek(range_start, os.SEEK_SET)
        return self._trace.buffer.read(TRACE_CHUNK_SIZE)

    def _buffer_len(self):
        self._trace.buffer.seek(0, os.SEEK_END)
        return self._trace.buffer.tell()
        
    def _close_trace(self):
        self._trace.buffer.close()
        self._trace = None
        
    def _run_trace(self):
        if self._trace is not None and not self._trace.paused:
            if self._trace.page == 0 and TRAIL_BY > 0 and self._buffer_len() / TRACE_CHUNK_SIZE > TRAIL_BY:
                self._trace.page = int(self._buffer_len() / TRACE_CHUNK_SIZE) - TRAIL_BY
            
            page = self._next_page()
            if page:
                payload = page[self._trace.offset:].decode()
                if payload:
                    self.write_json(id=None, result={
                        "source": "trace",
                        "project_id": self._trace.job.pipeline["project_id"],
                        "job_id": self._trace.job.id,
                        "payload": payload
                    })
                offset = len(page)
                if offset >= TRACE_CHUNK_SIZE or self._trace.finished:
                    self._trace.page += 1
                    offset = 0
                self._trace.offset = offset

            # Output full pages of cached trace before making the next request
            if page is not None and time_ms() - self._trace.ts > REQUEST_TIMEOUT and len(page) < TRACE_CHUNK_SIZE:
                self._trace.job.refresh()
                trace = self._trace.job.trace()
                self._trace.buffer.write(trace[self._buffer_len():])
                self._trace.buffer.flush()
                self._trace.length = len(trace)
                self._trace.finished = self._trace.job.finished_at is not None
                self._trace.ts = time_ms()

    def rpc_pipeline(self, project_id, pipeline_id):
        pipeline = self._run_pipeline(project_id, pipeline_id)
        self._requests.append((pipeline, self._run_pipeline, (project_id, pipeline_id)))
        self.write_json(id=None, result=pipeline)

        jobs = self._run_jobs(project_id, pipeline_id)
        self._requests.extend([(job, self._run_job, (project_id, job["payload"]["job_id"])) for job in jobs])
        self.write_json(id=None, result=jobs)

        return pipeline
        
    def rpc_trigger_pipeline(self, project_id, ref, variables=None):
        project = self._gl.projects.get(project_id, lazy=True)
        trigger_token_key = self._conf["projects"][project_id]["trigger_token_key"]
        pipeline = project.trigger_pipeline(ref, self._netrc.authenticators(trigger_token_key)[2], variables)

        return self.rpc_pipeline(project_id, pipeline.id)
        
    def rpc_cancel_pipeline(self, project_id, pipeline_id):
        project = self._gl.projects.get(project_id, lazy=True)
        pipeline = project.pipelines.get(pipeline_id)
        pipeline.cancel()

        self.rpc_pipeline(project_id, pipeline.id)

    def rpc_retry_pipeline(self, project_id, pipeline_id):
        project = self._gl.projects.get(project_id, lazy=True)
        pipeline = project.pipelines.get(pipeline_id)
        pipeline.retry()

        return self.rpc_pipeline(project_id, pipeline.id)

    def rpc_retry_job(self, project_id, job_id):
        project = self._gl.projects.get(project_id, lazy=True)
        job = project.jobs.get(job_id)
        job.retry()

        return self.rpc_pipeline(project_id, job.pipeline["id"])

    def rpc_job_trace(self, project_id, job_id):
        if self._trace and not (project_id == self._trace.job.pipeline["project_id"] and job_id == self._trace.job.id):
            self._close_trace()

        if self._trace:
            self._trace.page = 0
        else:
            project = self._gl.projects.get(project_id, lazy=True)
            job = project.jobs.get(job_id)
            self._trace = Trace(job)

    def rpc_pause_trace(self):
        if self._trace:
            self._trace.paused = True

    def rpc_resume_trace(self):
        if self._trace:
            self._trace.paused = False

    def rpc_quit_trace(self):
        self.rpc_pause_trace()
        
    def rpc_get_commit(self, project_id, commit_id):
        project = self._gl.projects.get(project_id, lazy=True)
        try:
            commit = project.commits.get(commit_id)
            return commit.id
        except GitlabGetError:
            return
        
    def send_notification(self):
        if time_ms() - self._request_ts > REQUEST_TIMEOUT:
            new_requests = []
            while len(self._requests) > 0:
                resp, request, args = self._requests.pop()
                if resp["payload"]["finished_at"] is None or \
                ("updated_at" in resp["payload"] and resp["payload"]["finished_at"] < resp["payload"]["updated_at"]) :
                    new_resp = request(*args)
                    if new_resp["payload"]["status"] != resp["payload"]["status"]:
                        self.write_json(id=None, result=new_resp)
                    new_requests.append((new_resp, request, args))
            self._requests = new_requests
            self._request_ts = time_ms()
        self._run_trace()
