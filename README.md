pipmon is an emacs major mode for triggering and monitoring Gitlab pipelines.

![pipmon buffer](https://raw.githubusercontent.com/champagst/pipmon/refs/heads/main/screenshot.PNG)

# Configuration

Connection parameters are configured in a yaml file, ~/.pipmon.yml

```yaml
url: https://gitlab.com
private_token_key: gitlab_token
projects:
  3077:
    trigger_token_key: trigger_token
```

Token keys are keys into a netrc file, ~/.authinfo

```
machine gitlab_token password <your_access_token>
machine trigger_token password <project_trigger_token>
```

# Use

`pipmon` turns a buffer into a listing of previously triggered pipelines.

`(pipmon-trigger id ref &key note vars)` triggers a pipeline.

It's most conveniently used in a user defined function

```elisp
(cl-defun ops/model (ref model &key note vars)
  (funcall
    'pipmon-trigger
      <id> ref :note note
      :vars (append vars
              (list
	      "_PIPELINE_FILE_NAME" "dbt-ci.yml"
	      "DBT_MODEL" model))))
```

which can be run using `(pipmon-run '(ops/tbl "main" "model"))`, making a self-describing note for the pipeline that can be reran with `pipmon-rerun` as a form of `elisp--preceding-sexp`.

pipmon-pipeline-toggle - toggle visability of the jobs of a pipeline
pipmon-delete-pipeline - delete the pipeline entry from the buffer, the pipeline itself is unaffected
pipmon-cancel-pipeline - cancel a pipeline
pipmon-retry-pipeline  - retry a pipeline

pipmon-retry-job       - retry a pipeline job
pipmon-log             - switch to a buffer displaying the log of a pipeline job
