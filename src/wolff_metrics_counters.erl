-module(wolff_metrics_counters).


-export([
    batching_inc/1,
    batching_inc/2,
    dropped_inc/1,
    dropped_inc/2,
    dropped_other_inc/1,
    dropped_other_inc/2,
    dropped_queue_full_inc/1,
    dropped_queue_full_inc/2,
    dropped_queue_not_enabled_inc/1,
    dropped_queue_not_enabled_inc/2,
    dropped_resource_not_found_inc/1,
    dropped_resource_not_found_inc/2,
    dropped_resource_stopped_inc/1,
    dropped_resource_stopped_inc/2,
    failed_inc/1,
    failed_inc/2,
    inflight_inc/1,
    inflight_inc/2,
    matched_inc/1,
    matched_inc/2,
    queuing_inc/1,
    queuing_inc/2,
    retried_inc/1,
    retried_inc/2,
    retried_failed_inc/1,
    retried_failed_inc/2,
    retried_success_inc/1,
    retried_success_inc/2,
    success_inc/1,
    success_inc/2
]).


batching_inc(_ID) ->
    ok.

batching_inc(_ID, _Val) ->
    ok.

dropped_inc(_ID) ->
    ok.

dropped_inc(_ID, _Val) ->
    ok.

dropped_other_inc(_ID) ->
    ok.

dropped_other_inc(_ID, _Val) ->
    ok.

dropped_queue_full_inc(_ID) ->
    ok.

dropped_queue_full_inc(_ID, _Val) ->
    ok.

dropped_queue_not_enabled_inc(_ID) ->
    ok.

dropped_queue_not_enabled_inc(_ID, _Val) ->
    ok.

dropped_resource_not_found_inc(_ID) ->
    ok.

dropped_resource_not_found_inc(_ID, _Val) ->
    ok.

dropped_resource_stopped_inc(_ID) ->
    ok.

dropped_resource_stopped_inc(_ID, _Val) ->
    ok.

matched_inc(_ID) ->
    ok.

matched_inc(_ID, _Val) ->
    ok.

queuing_inc(_ID) ->
    ok.

queuing_inc(_ID, _Val) ->
    ok.

retried_inc(_ID) ->
    ok.

retried_inc(_ID, _Val) ->
    ok.

failed_inc(_ID) ->
    ok.

failed_inc(_ID, _Val) ->
    ok.

inflight_inc(_ID) ->
    ok.

inflight_inc(_ID, _Val) ->
    ok.

retried_failed_inc(_ID) ->
    ok.

retried_failed_inc(_ID, _Val) ->
    ok.

retried_success_inc(_ID) ->
    ok.

retried_success_inc(_ID, _Val) ->
    ok.

success_inc(_ID) ->
    ok.

success_inc(_ID, _Val) ->
    ok.

