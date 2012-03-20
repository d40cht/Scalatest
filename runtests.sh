#!/bin/sh
./sbt test
./sbt "run tests/typeable.pacatoon tests/pairsUsingPartialFunctions.pacatoon tests/test1.pacatoon tests/closuretests.pacatoon"
