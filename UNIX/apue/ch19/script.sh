#!/bin/sh
date > typescript
pty "${SHELL:-/bin/sh}" | tee typescript
date >> typescript
