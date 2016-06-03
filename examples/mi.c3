// here are the model's channels
channels: c1,c2,c3; 


// and here are the model's networks
networks: ordered n1{c1}, unordered n2{c1,c2};

// in the curly brackets there are the channels belonging
// to the network
// networks can either be ordered (FIFO queue) or unordered
// (multisets of messages)



machine Cache:[4] {

  // self-issued msgs are "Rules"
  // not in the "Cache functions" part
  // i.e. the receive function

  (I, *Read_Write, IM) {
    	Dir[0]!GetM;
  }

  (M, *Read_Write) {    // ignore the responses that have only
    	hit;		 // strings in their body. Not contributing
  }			 // to the model

  (IM, *Read_Write) {
    	stall;
  }

  (IM, src?Data, M) {
	Dir[0]!Ack;
  }

  (M, Dir[0]?Fwd_GetM<cache src1, dir dir>,I) {
   	 src1!Data;
  }

}

machine Dir:[1] {
 []int[1..10] nums,  // integers must have ranges
 boolean working, ack {sent,received,waiting}, 
 []boolean staff, [][]Cache caches;
  (I, src?GetM,IM) {
    	owner = src;
	src!Data;
  }

  (M, src?GetM,IM) {
    owner!Fwd_GetM<cache src, dir dir>; // in murphi: Send(Fwd_GetM, owner, src)
    owner = src;	   // the argument of the message becomes the sender
  }

  (IM, src?GetM ) {
    stall;
  }

  (IM, src?Ack, M) {
   }
}
