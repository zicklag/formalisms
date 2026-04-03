#!/bin/env sh

quint repl \
  --backend typescript \
  -r survey.qnt::roomy \
  'permissionedSpacesMissing' \
  'stratosMissing' \
  'opensocialMissing' \
  'allMissing' \
  '.exit'