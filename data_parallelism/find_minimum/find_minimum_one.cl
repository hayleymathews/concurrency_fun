__kernel void find_minimum_one(__global const float* values,
                               __global float* result,
                               __local float* scratch) {
    int i = get_global_id(0);
    int n = get_global_size(0);

    scratch[i] = values[i];

    barrier(CLK_LOCAL_MEM_FENCE);

    for (int j = n / 2; j > 0; j /= 2) {
        if (i < j)
            scratch[i] = min(scratch[i], scratch[i + j]);
        barrier(CLK_LOCAL_MEM_FENCE);
    }

    if (i == 0)
        *result = scratch[0];

}