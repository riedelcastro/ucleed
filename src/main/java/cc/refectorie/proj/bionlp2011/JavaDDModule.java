package cc.refectorie.proj.bionlp2011;

/**
 * @author sriedel
 */
public interface JavaDDModule {
    //calculate, say, MST with the given penalties on each edge
    Message infer(Message penalties);
}


interface Message {
    //value associated with given trigger and label. 1.0 if true, 0.0 if false. Can also
    //be used to specify real-valued penalties
    double msg(TriggerID trigger, String label);
    //same for edges
    double msg(EdgeID edge, String role);

    //all triggers for which this message provides information for
    Iterable<TriggerID> triggers();
    //all edges for which this message provides information for
    Iterable<EdgeID> edges();
}

interface TriggerID {
    //the end character offset of the span corresponding to this trigger
    int endOffset();
}

interface EdgeID {
    //the end character offset of the arg1 (trigger) of this edge
    int arg1EndOffset();
    //the end character offset of the arg2 (argument) of this edge
    int arg2EndOffset();
}