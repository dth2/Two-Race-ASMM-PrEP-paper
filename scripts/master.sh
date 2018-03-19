#!/bin/bash

qsub -q batch -t 1-12  -v SIMNO=1 runsim.sh
qsub -q bf -t 1-12  -v SIMNO=2 runsim.sh

