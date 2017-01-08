// This example is based on the mi protocol but diverges significantly from it
// to give examples of most (if not all) features of the language




// global variables
// each declaration is of type TypeDecl,
// just like the arguments of messages or 
// the field declarations of machines
// global: int[0..1000] msgsExchanged; not supported in Backend at this stage


// and here are the model's networks
networks: ordered net1{c1}, unordered net2{c1,c2};

// in the curly brackets there are the channels belonging
// to the network
// networks can either be ordered (FIFO queue) or unordered
// (multisets of messages)

// we may have global variables see note in the end





machine Cache[4] {

   startstate: I;			// the starting state is I

  // self-issued msgs are "Rules"
  // not in the "Cache functions" part
  // i.e. the receive function

  (I, *Read_Write, IM) {
    	Dir[0]!GetM@c1;
  }

  (M, *Read_Write) {    // ignore the responses that have only
    	hit;		      // strings in their body. Not contributing
  }			      // to the model

  (IM, *Read_Write) {
    	stall;		
  }			    

  (IM, src?Data@c1, M) {
	// you can set the values of local params like this	
	int[0..10] integer = 5 + 3/2; 
	Dir[0]!Ack@c2;
  }

  (M, Dir[0]?Fwd_GetM<Cache mod=src1, node direct=dir>, I) {   // we can infer which variables
   	 src1!Data@c1;  				                 // are msg args see note in the end
  }

}

machine Dir {	               // because it has size 1, it will be
 		                    // parsed as nonsymmetric

startstate: I; 
	
 int[1..10] nums,      		 // initial value undefined

 [5]boolean working (false) ,    // all elements are set to false at startstate

 ack {sent,received,waiting} (sent) , // enum with initial value 'sent'

 [Cache] boolean acks,               // acks is indexed by the scalarset/enum 
                                    // cache states are indexed too

 set[Cache] Cache modifiers; 	// the size of the set is #caches
 	

      

  (I, src?GetM, IM) {

	working[1] = true;   
    	owner = src;
	modifiers.add(src);	     
	src!Data@c1;

  }



  (M, src?GetM, IM) {

    owner!Fwd_GetM<node direct>@c1; 
    Dir thisDirectory = direct;
    owner = src;	   // the argument of the message becomes the sender
    modifiers.del(src);

  }




  (IM, src?GetM@c1 ) {

    modifiers.add(src);
    modifiers!NewModifier@c2; 

   // broadcast to a set
   // msg received by every machine in the set
   // if typeOf set != machine, error at some point

   }


  (IM, src?Ack@c2, M) {

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
