-module(wolff_metrics).


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


batching_inc(Config) ->
    batching_inc(Config, 1).

batching_inc(Config, Val) ->
    telemetry:execute([wolff, batching],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

dropped_inc(Config) ->
    dropped_inc(Config, 1).

dropped_inc(Config, Val) ->
    telemetry:execute([wolff, dropped],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

dropped_other_inc(Config) ->
    dropped_other_inc(Config, 1).

dropped_other_inc(Config, Val) ->
    telemetry:execute([wolff, dropped_other],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

dropped_queue_full_inc(Config) ->
    dropped_queue_full_inc(Config, 1).

dropped_queue_full_inc(Config, Val) ->
    telemetry:execute([wolff, dropped_queue_full],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

dropped_queue_not_enabled_inc(Config) ->
    dropped_queue_not_enabled_inc(Config, 1).

dropped_queue_not_enabled_inc(Config, Val) ->
    telemetry:execute([wolff, dropped_queue_not_enabled],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

dropped_resource_not_found_inc(Config) ->
    dropped_resource_not_found_inc(Config, 1).

dropped_resource_not_found_inc(Config, Val) ->
    telemetry:execute([wolff, dropped_resource_not_found],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

dropped_resource_stopped_inc(Config) ->
    dropped_resource_stopped_inc(Config, 1).

dropped_resource_stopped_inc(Config, Val) ->
    telemetry:execute([wolff, dropped_resource_stopped],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

matched_inc(Config) ->
    matched_inc(Config, 1).

matched_inc(Config, Val) ->
    telemetry:execute([wolff, matched],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

queuing_inc(Config) ->
    queuing_inc(Config, 1).

queuing_inc(Config, Val) ->
    telemetry:execute([wolff, queuing],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

retried_inc(Config) ->
    retried_inc(Config, 1).

retried_inc(Config, Val) ->
    telemetry:execute([wolff, retried],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

failed_inc(Config) ->
    failed_inc(Config, 1).

failed_inc(Config, Val) ->
    telemetry:execute([wolff, failed],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

inflight_inc(Config) ->
    inflight_inc(Config, 1).

inflight_inc(Config, Val) ->
    telemetry:execute([wolff, inflight],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

retried_failed_inc(Config) ->
    retried_failed_inc(Config, 1).

retried_failed_inc(Config, Val) ->
    telemetry:execute([wolff, retried_failed],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

retried_success_inc(Config) ->
    retried_success_inc(Config, 1).

retried_success_inc(Config, Val) ->
    telemetry:execute([wolff, retried_success],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

success_inc(Config) ->
    success_inc(Config, 1).

success_inc(Config, Val) ->
    telemetry:execute([wolff, success],
                      #{counter_inc => Val},
                      telemetry_meta_data(Config)).

telemetry_meta_data(Config) ->
    #{data => maps:get(telemetry_meta_data, Config,  default_data)}.

