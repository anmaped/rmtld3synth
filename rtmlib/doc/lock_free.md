
# RTEMLib lock-free and wait-free ring buffer

The wait-free and lock-free ring buffers is a useful technique for time and memory sensitive systems such as the real-time systems. The wait-free nature of the buffer gives a fixed number of steps for each operation, and the lock-free nature of the buffer enables two or more threads communication.

\image html ring_buffer.png

Given that, we can ensure liveness properties of the runtime monitoring library and any deadlock from lock of resources is avoided by contruction.

[TODO]
