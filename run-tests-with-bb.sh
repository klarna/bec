#!/bin/bash

export BB_STAGING_URL=http://localhost:7990
export BB_STAGING_PROJECT_KEY=TOOLS
export BB_STAGING_REPO_SLUG=bec-test
export BB_STAGING_TEAM_A=team.a
export BB_STAGING_TEAM_B=team.b
export BB_STAGING_USER_A=user.a
export BB_STAGING_USER_B=user.b
export BB_STAGING_USERNAME=admin
export BB_STAGING_PASSWORD=admin

rebar3 proper
