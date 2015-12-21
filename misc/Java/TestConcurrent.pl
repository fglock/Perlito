package Java::Thread {
    import => "java.lang.Thread"
};

package ConcurrentLinkedQueue::Of::String {
    import    => "java.util.concurrent.ConcurrentLinkedQueue",
    java_type => "ConcurrentLinkedQueue<PlObject>",
};

my $global_queue = new ConcurrentLinkedQueue::Of::String();

my Java::Thread $producer = new Java::Thread(
    sub {
        my ConcurrentLinkedQueue::Of::String $queue = $global_queue->to_ConcurrentLinkedQueueOfString();
        my $x = 1;
        while(1) {
            $x++;
            $queue->add($x);   
            sleep (1);
        }
    }
);
my Java::Thread $consumer = new Java::Thread(
    sub {
        while(1) {
            my ConcurrentLinkedQueue::Of::String $queue = $global_queue->to_ConcurrentLinkedQueueOfString();
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

