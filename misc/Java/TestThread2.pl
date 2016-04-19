package Java::Thread {
    import => "java.lang.Thread"
};

my $shared = 0;

eval {
    my Java::Thread $thread1 = Java::Thread->new(
        sub {
            for my $i (0..5) {
                $shared++;  # unsafe!
                print "thread 1; shared $shared\n";
                sleep (1);
            }
        }
    );
    my Java::Thread $thread2 = Java::Thread->new(
        sub {
            for my $i (0..5) {
                $shared++;  # unsafe!
                print "thread 2; shared $shared\n";
                sleep (1);
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

print "done. shared $shared\n";

