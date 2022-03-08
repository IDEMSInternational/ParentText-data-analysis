# pip install rapidpro-python

from temba_client.v2 import TembaClient

f = open("tokens\PT_malaysia_key.txt", "r")
token = f.read()
client = TembaClient('https://app.rapidpro.io', token)

 



for flows in client.get_flows().iterfetches(retry_on_rate_exceed=True):
    for flow in flows:
        if (flow.name == "PLH - Welcome - Entry"):
            supp_uuid = flow.uuid
         
            print(flow.runs.serialize())


n_expired = 0
n_completed = 0      

for runs in client.get_runs(flow= supp_uuid).iterfetches(retry_on_rate_exceed=True):
    print(len(runs))
    
    for run in runs:
        print(run.created_on)
        if run.exit_type == "completed":
            n_completed +=1
        elif run.exit_type == "expired":
            n_expired +=1

    print(n_completed)
    print(n_expired)
    
