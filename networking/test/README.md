# networking/test

The networking package's tests, run by `flutter test` in `networking/` and by Section NET of `test/run_all_tests.sh`.

## Tests that need IPv6

Three tests of `transport/hole_punch_service_test.dart` --- "punch sends packets to target address", "punch completes after duration" and "punch can be cancelled via dispose" --- bind a UDP socket on the IPv6 loopback, `::1`.  They pass where the host has IPv6, as on the Mac (Section NET on main 1ecdd1e8, 839 of 839).  They fail in a cloud container, whose kernel has no IPv6 (no `/proc/net/if_inet6`): the bind raises `SocketException: Failed to create datagram socket (OS Error: Address family not supported by protocol, errno = 97), address = ::1`, and the test's `service` is then never set (`LateInitializationError`).  A run in a cloud container is read with these three failures as the container's, not the code's.
