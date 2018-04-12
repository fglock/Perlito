package Java::Thread { import => "java.lang.Thread" };

package Java::AtomicInteger { import => "java.util.concurrent.atomic.AtomicInteger" };

my $shared = 0;
my $atomic = Java::AtomicInteger->new(0);

eval {
    my Java::Thread $thread1 = Java::Thread->new(
        sub {
            for my $i ( 0 .. 5 ) {
                $shared++;                                             # unsafe!
                $atomic->incrementAndGet();    # safe
                print "thread 1; shared $shared\n";
                sleep(1);
            }
        }
    );
    my Java::Thread $thread2 = Java::Thread->new(
        sub {
            for my $i ( 0 .. 5 ) {
                $shared++;                                             # unsafe!
                $atomic->incrementAndGet();    # safe
                print "thread 2; shared $shared\n";
                sleep(1);
            }
        }
    );

    $thread1->start();
    $thread2->start();

    $thread1->join();
    $thread2->join();
    1;
}
  or die $@;

my $value = $atomic->get();
print "done. shared $shared (unsafe) - $value (safe)\n";

