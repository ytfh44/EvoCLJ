# Host Polling Protocol (component)

## Overview

This document describes the host polling protocol for EvoCLJ deployment
status. The protocol is used by external hosts (CLI, CI/CD, dashboard)
to observe the current canary deployment state without mutating it.

## Polling Endpoint

The host polls the deployment state through the public read path:

    GET /api/deployment/current

or, in the CLI:

    evoclj deploy current

## Response Shape

    {:generation/id <current-generation-id>
     :genome/id <content-address>
     :canary {:generation <canary-generation-id>
              :allocation <fraction>
              :active? <bool>}
     :timestamp <ISO-8601>}

## Polling Frequency

- Recommended: every 30 seconds during active rollout.
- Back off to 5 minutes once `:canary/:allocation` reaches 1.0 or
  `:active?` is false.

## Failure Semantics

- 404 / empty response → no deployment state yet; host falls back to
  the seed generation.
- 500 → retry with exponential backoff (max 5 minutes).

## Observability

Every poll is a read-only query against the SQLite generations table
(current = 1). No writes, no locks, no promotion side effects.
