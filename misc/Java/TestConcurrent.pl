package Java::Thread {
    import => "java.lang.Thread"
};

package ConcurrentLinkedQueue::Of::String {
    import    => "java.util.concurrent.ConcurrentLinkedQueue<PlObject>",
};

my $global_queue = ConcurrentLinkedQueue::Of::String->new();

my Java::Thread $producer = Java::Thread->new(
    sub {
        my ConcurrentLinkedQueue::Of::String $queue = $global_queue;
        my $x = 1;
        while(1) {
            $x++;
            $queue->add($x);   
            sleep (1);
        }
    }
);
my Java::Thread $consumer = Java::Thread->new(
    sub {
        while(1) {
            my ConcurrentLinkedQueue::Of::String $queue = $global_queue;
            my $x = $queue->poll();
            if (defined $x) {
                print "consumer got $x\n";
            }
            sleep (1);
        }
    }
);

$producer->start();
$consumer->start();

sleep(1);
say "done";

