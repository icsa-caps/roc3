
// global variables
// each declaration is of type TypeDecl,
// just like the arguments of messages or 
// the field declarations of machines
global: int[0..1000] msgsExchanged;

// here are the model's channels
channels: c1,c2,c3; 

// and here are the model's networks
networks: ordered net1{c1}, unordered net2{c1,c2};

// in the curly brackets there are the channels belonging
// to the network
// networks can either be ordered (FIFO queue) or unordered
// (multisets of messages)

// we may have global variables see note in the end





machine Cache:[4] {
  // self-issued msgs are "Rules"
  // not in the "Cache functions" part
  // i.e. the receive function

  (I, *Read_Write, IM) {
    	Dir[0]!GetM;
  }

  (M, *Read_Write) {    // ignore the responses that have only
    	hit;		      // strings in their body. Not contributing
  }			      // to the model

  (IM, *Read_Write) {
    	stall;		
  }			    

  (IM, src?Data, M) {
	Dir[0]!Ack;
  }

  (M, Dir[0]?Fwd_GetM<cache src1, dir dir>, I) {   // we can infer which variables
   	 src1!Data;  				          // are msg args see note in the end
  }

}

machine Dir:[1] {
 int[1..10] nums,      		// integers must have ranges

 [5]boolean working,   		// arrays are indexed by nums

 ack {sent,received,waiting},

 [Cache] boolean acks,    		// or by a machine, 
					      // in which case they are maps

 set:Cache Cache modifiers, 	// the size of the set is #caches

 set:5 boolean other; 	      // the set can store up to 5 elements

  (I, src?GetM, IM) {   
    	owner = src;
	modifiers.add(src);	     
	src!Data;
  }

  (M, src?GetM,IM) {
    owner!Fwd_GetM<cache src, dir dir>; // in murphi:Send(Fwd_GetM, owner, src)
    owner = src;	   // the argument of the message becomes the sender
    modifiers.del(src);
  }

  (IM, src?GetM ) {
    modifiers.add(src);
    broadcast(dir, modifiers, NewModifier<cache src>); // broadcast to a set
    stall;					         // msg received by every
						         // machine in the set
  }						         // if typeOf set != machine, 
						         // error at some point


  (IM, src?Ack, M) {
   }
}




// when accessing a variable in murphi inside a receive function of a machine,
// we need to know whether the variable is a filed of the machine (print node.<var>),
// a field of the message being processed (print msg.<var>) or a global variable
// (print <var>).
// at the intermidiate stage we can do the following:
// see if the variable is in the list of global variables. If it's not, see if it's 
// one of the message arguments. If it's neither a message argument nor a global
// variable, then it's a field of the machine
