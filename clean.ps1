#!/usr/bin/env pwsh

$ErrorActionPreference = "Stop"
Set-StrictMode -Version 2.0

Remove-Item -ErrorAction Ignore -Force -Recurse build
Remove-Item -ErrorAction Ignore -Force -Recurse obj
