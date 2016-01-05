
# RTEMLib Task Helpers

## Creating tasks

RTEMLib contains helpers to aid the creation of tasks. __task is the type of the task descriptor.

```__task``` should be used to initialize task helpers as follows:

~~~~~~~{.cpp}
__task dummy_task = __task(f, pri, pol, p_us);

~~~~~~~
where `f` is the function to be executed periodically or sporadically, `pri` is the priority, `pol` is the policy, and `p_us` the period in microseconds.

