#!/bin/bash
set -e

cd "$(dirname "$0")"
lake build timekeeper
.lake/build/bin/timekeeper "$@"
