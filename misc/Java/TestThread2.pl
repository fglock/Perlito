package Java::Thread {
    import => "java.lang.Thread"
};

eval {
    my Java::Thread $thread1 = Java::Thread->new(
        sub {
            for my $i (0..5) {
                print "thread 1\n";
                sleep (1);
            }
        }
    );
    my Java::Thread $thread2 = Java::Thread->new(
        sub {
            for my $i (0..5) {
                print "thread 2\n";
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

print "done\n";

