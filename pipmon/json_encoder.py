import json

class JSONEncoder(json.JSONEncoder):
    def default(self, o):
        return super().default(o)
