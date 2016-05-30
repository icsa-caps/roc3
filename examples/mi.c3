
BEGIN
machine Cache[4] {
  (I, Issue(Read_Write)) {
    Send(GetM,Dir0);
    Trans(IM);
  }

  (M, Issue(Read_Write)) {
    hit;
  }

  (M, Receive(Fwd_GetM<src1>)) {
    Send(Data,src1);
    Trans(I);
  }

  (IM, Issue(Read_Write)) {
    stall;
  }

  ( IM, Receive(Data,src)) {
    Send(Ack,src);
    Trans(M);
  }
}

machine Dir[1] {
  (I, Receive(GetM,src)) {
    owner = src;
    Send(Data,src);
    Trans(IM);
  }

  (M, Receive(GetM,src)) {
    Send(Fwd_GetM<src>, owner);
    owner = src;
    Trans(IM);
  }

  (IM, Receive(GetM) ) {
    stall;
  }

  (IM, Receive(Ack)) {
     Trans(M);
   }
}

END

