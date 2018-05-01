#!/bin/bash

rm -rf ./*.beam

erlc fan_controller.erl

echo " c(fan_controller). dbg:tracer(). dbg:tpl(fan_controller, '_', []). dbg:p(new, m). fan_controller:main()." | erl