#!/usr/bin/env python3
"""
Makes sure:

 * All jobs are listed in the 'all' job
 * Only existing tests are listed

"""

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import networkx as nx
import sys
import yaml

CI_PATH = ".github/workflows/ci.yml"
ALL_TEST = "all"

def get_needs(job_data):
  needs = job_data.get("needs", ())
  if not isinstance(needs, list):
    needs = [needs]
  return needs

def main():
  ci_yml_fp = open(CI_PATH, "r")
  ci_yml_parsed = yaml.load(ci_yml_fp, Loader=yaml.FullLoader)

  needs_graph = nx.DiGraph()
  needs_graph.add_nodes_from((job, {"needs" : get_needs(data)}) for job, data in ci_yml_parsed['jobs'].items())
  needs_graph.add_edges_from([(j, n) for j, d in needs_graph.nodes(data=True) for n in d["needs"]])

  all_jobs = set(needs_graph.nodes) - {ALL_TEST}
  all_needs = set(nx.descendants(needs_graph, ALL_TEST))

  if all_jobs - all_needs:
    sys.exit(f"Not all jobs mentioned in {ALL_TEST}.needs: {all_jobs - all_needs}")

  if all_needs - all_jobs:
    sys.exit(f"Non-existing jobs found in {ALL_TEST}.needs: {all_needs - all_jobs}")


if __name__ == '__main__':
  main()
