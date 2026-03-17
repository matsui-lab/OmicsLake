\name{ol_enable_strict_repro_mode}
\alias{ol_enable_strict_repro_mode}
\title{Enable Strict Reproducibility Mode}
\usage{
ol_enable_strict_repro_mode(
  path = getwd(),
  prompt_id = NULL,
  run_id = NULL,
  agent_name = NULL,
  include = c("git", "renv", "session", "system"),
  snapshot_validate_mode = c("error", "warn", "off"),
  max_tables = 200L
)
}
\arguments{
\item{path}{Base path used for Git/renv detection (usually repository root)}

\item{prompt_id}{Optional AI prompt/work-item identifier}

\item{run_id}{Optional AI run/session identifier}

\item{agent_name}{Optional AI agent name}

\item{include}{Metadata components to capture}

\item{snapshot_validate_mode}{Snapshot validation behavior: "error", "warn", or "off"}

\item{max_tables}{Maximum tables stored in snapshot validation details}
}
\value{
Named list of previous option values (invisible)
}
\description{
Applies an opinionated, audit-first option preset for AI-assisted analysis:
reproducibility metadata capture ON, clean-git requirement ON, and
snapshot validation ON.
}
\examples{
# Minimal runnable example for BiocCheck
x <- 1
x

if (FALSE) {
ol_enable_strict_repro_mode(
  path = getwd(),
  prompt_id = "prompt-2026-02-21-001"
)
}
}
