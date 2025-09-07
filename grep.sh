#!/bin/bash

grep -r '\bif\b' hs/src/HelVM/HelPS || true
grep -r '\bcase\b' hs/src/HelVM/HelPS || true
grep -r '\blet\b' hs/src/HelVM/HelPS || true
grep -r '\bdo\b' hs/src/HelVM/HelPS || true
