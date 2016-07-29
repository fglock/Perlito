sub transform {
    my $column = shift;

    # Take care of types (unwrap Text object, cast to String, wrap back to perl var)
    # TODO: generate this automatically based on UDF signature
    my $pString = $column->to_Text()->toString();

    # do the work
    my @checkin = $pString=~m/checkin=(\d{4}-\d{2}-\d{2})/g;

    # cast back to wraped Text & return
    # TODO: generate this automatically based on UDF signature
    my $result = Text->new(join("-", @checkin));
    return $result;
}
