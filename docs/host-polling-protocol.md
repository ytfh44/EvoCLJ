# Host Polling Protocol (component)

## Overview

This document describes the host polling protocol for EvoCLJ deployment
status. The protocol is used by external hosts (CLI, CI/CD, dashboard)
to observe the current canary deployment state without mutating it.

## Polling Endpoint

> **Verification note (2026-09):** the endpoint and CLI below are
> implemented and covered: `evoclj.http.api-test`
> (`deployment-current-*`) pins the HTTP shapes and
> `evoclj.cli.deploy-test` (`deploy-current-*`) pins the CLI shapes.

The host polls the deployment state through the public read path:

    GET /api/deployment/current

or, in the CLI:

    evoclj deploy current

Both return the same shape (below). Both are read-only: one SELECT
against the SQLite generations table (`current = 1`), no writes, no
locks, no promotion side effects. `deploy current` records no deploy
decision; `deploy <generation-id>` is the separate mutating command.

## Response Shape

    {:generation/id <current-generation-id>
     :genome/id <content-address>
     :canary nil
     :timestamp <ISO-8601>}

`:canary` is nil: rollout allocations live in the deployment-state
envelopes returned by `evoclj deploy <generation-id>`, not in the
generations table. Nil means no rollout is configured, which the
canary contract treats as route-everything-to-current — never an
error. A future rollout that persists allocation steps extends this
field; the nil case stays the no-rollout default.

## Polling Frequency

- Recommended: every 30 seconds during active rollout.
- Back off to 5 minutes once the rollout is complete or no canary is
  configured.

## Failure Semantics

- 404 / `:deployment/no-current-generation` (HTTP) or
  `:cli/no-current-generation` (CLI) → no deployment state yet; host
  falls back to the seed generation.
- 500 → retry with exponential backoff (max 5 minutes). A 500 with
  `:http/store-unavailable` is a host wiring bug (no sqlite store on
  the request's `:system`) — fix the server wrapper, do not retry
  blindly.

## Observability

Every poll is a read-only query against the SQLite generations table
(current = 1). No writes, no locks, no promotion side effects.
