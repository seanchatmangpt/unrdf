import json
from pathlib import Path

def test_propagation_plan():
    data=json.loads(Path('generated/propagation/plan.json').read_text())
    assert data['schema']=='ggen.capability-lineage-propagation/1'
    assert data['actuation_performed'] is False
    assert data['force_push_allowed'] is False
    assert all(plan['edges'] >= 1 for plan in data['plans'])
