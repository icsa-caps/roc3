// This is the MESI protocol from Sorin/Hill/Wood





networks: ordered fwdNet {fwd},    //FwdGetS, FwdGetM, Inv, PutAck
          unordered respNet{resp}, // Data, InvAck
          unordered reqNet {req};  //GetS, GetM, PutM


machine cache[3] {


startstate: I;


int[0..5] acksReceived (0),
int[0..5] acksExpected (0);


(I, *load, IS_D){

	Dir[0]!GetS@req;
}



(I, *store, IM_AD){
    Dir[0]!GetM@req;
    acksReceived = 0;
    acksExpected = 0;

}


// IS_D

(IS_D, *load){
	stall;
}

(IS_D, *store){
	stall;
}

(IS_D, *replace){
	stall;
}

(IS_D, src?Inv){
	stall;
}

(IS_D, src?FwdGetS){
    stall;
}

(IS_D, src?FwdGetM){
    stall;
}

(IS_D, src?Data<boolean exclusive> & exclusive==false, S){
}

(IS_D, src?Data<boolean exclusive> & exclusive==true, E){ 
}



(IM_AD, *load){
	stall;
}

(IM_AD, *store){
	stall;
}

(IM_AD, *replace){
	stall;
}

(IM_AD, src?FwdGetS){
	stall;
}

(IM_AD, src?FwdGetM){
	stall;
}



(IM_AD, Dir[0]?Data<int[0..5] ackCount> & ackCount == 0, M){


}

(IM_AD, Dir[0]?Data<int[0..5] ackCount> & ackCount > 0 & ackCount != acksReceived , IM_A){
	
       acksExpected = ackCount;
}





(IM_AD, src?Data, M){ // the src must be the owner of the block
}

(IM_AD, src?InvAck & acksExpected != acksReceived + 1){ // not last InvAck
	acksReceived = acksReceived + 1;
}


(IM_AD, src?InvAck & acksExpected == acksReceived + 1, M){ // last InvAck
	acksReceived = acksReceived + 1;
}






(IM_A, *load){
	stall;
}

(IM_A, *store){
	stall;
}

(IM_A, *replace){
	stall;
}

(IM_A, src?FwdGetS){
	stall;
}

(IM_A, src?FwdGetM){
	stall;
}


(IM_A, src?InvAck & acksExpected != acksReceived + 1){ 
	acksReceived = acksReceived + 1;

}

(IM_A, src?InvAck & acksExpected == acksReceived + 1, M){ 
    acksReceived = acksReceived + 1;
}




(S, *load){
	hit;
}

(S, *store, SM_AD){
    Dir[0]!GetM@req;
    acksReceived = 0;
    acksExpected = 0;
}

(S, *replace, SI_A){
    Dir[0]!PutS@req; // makes sense to be in req vc
                     // not specified in the book
    acksReceived = 0;
    acksExpected = 0;
}

(S, src?Inv<cache requestor>, I){ // requestor sent initially
                                  // GetM to Dir
	requestor!InvAck@resp;
}




(SM_AD, *load){
	hit;
}

(SM_AD, *store){
	stall;
}

(SM_AD, *replace){
	stall;
}

(SM_AD, src?FwdGetS){
	stall;
}

(SM_AD, src?FwdGetM){
	stall;
}

(SM_AD, src?Inv<cache requestor>, IM_AD){
	requestor!InvAck@resp;
}


(SM_AD, Dir[0]?Data<int[0..5] ackCount> & ackCount==0, M){
}

(SM_AD, Dir[0]?Data<int[0..5] ackCount> & ackCount > 0 & ackCount == acksReceived , M){
	
}

(SM_AD, Dir[0]?Data<int[0..5] ackCount> & ackCount > 0, SM_A){
	acksExpected = ackCount;
}


(SM_AD, src?Data, M){ // src == owner
}


(SM_AD, src?InvAck){
	acksReceived = acksReceived + 1;
}





(SM_A, *load){
	hit;
}

(SM_A, *store){
	stall;
}

(SM_A, *replace){
	stall;
}

(SM_A, src?FwdGetS){
	stall;
}

(SM_A, src?FwdGetM){
	stall;
}

(SM_A, src?InvAck & acksExpected != acksReceived + 1){
	acksReceived = acksReceived +1;
}

(SM_A, src?InvAck & acksExpected == acksReceived + 1, M){
    acksReceived = acksReceived + 1;
}





(M, *load){
	hit;
}

(M, *store){
	hit;
}

(M, *replacement, MI_A){
    Dir[0]!PutM@req;
    acksReceived = 0;
    acksExpected = 0;
}

(M, src?FwdGetS<cache requestor >, S){
    requestor!Data<boolean exclusive = false>@resp;
    Dir[0]!Data@resp;
}

(M, src?FwdGetM<cache requestor>, I){
    requestor!Data<boolean exclusive = false>@resp;
}



(E, *load){
	hit;
}

(E, *store, M){
	hit;
}

(E, *replacement, EI_A){
    Dir[0]!PutE@req;
}

(E, src?FwdGetS<cache requestor >, S){
    requestor!Data<boolean exclusive = false>@resp;
    Dir[0]!Data@resp;
}

(E, src?FwdGetM<cache requestor>, I){
    requestor!Data<boolean exclusive = false>@resp;
}


(MI_A, *load){
   hit;
}

(MI_A, *store){
   stall;
}

(MI_A, *replace){
   stall;
}

(MI_A, src?FwdGetS<cache requestor>, SI_A){
   requestor!Data<boolean exclusive = false>@resp;
   Dir[0]!Data@resp;
}

(MI_A, src?FwdGetM<cache requestor>, II_A){
   requestor!Data<boolean exclusive = false>@resp;
}

(MI_A, src?PutAck, I){
}



(EI_A, *load){
   hit;
}

(EI_A, *store){
   stall;
}

(EI_A, *replace){
   stall;
}

(EI_A, src?FwdGetS<cache requestor>, SI_A){
   requestor!Data<boolean exclusive = false>@resp;
   Dir[0]!Data@resp;
}

(EI_A, src?FwdGetM<cache requestor>, II_A){
   requestor!Data<boolean exclusive = false>@resp;
}

(EI_A, src?PutAck, I){
}



(SI_A, *load){
	hit;
}

(SI_A, *store){
	stall;
}

(SI_A, *replace){
	stall;
}


(SI_A, src?Inv< cache requestor >, II_A){
	requestor!InvAck@resp;
}

(SI_A, src?PutAck, I){
}





(II_A, *load){
	hit;
}

(II_A, *store){
	stall;
}

(II_A, *replace){
	stall;
}


(II_A, src?PutAck, I){
}



// cache declarations ends here

}





machine Dir {


startstate: I;

set[cache] cache sharers,
cache owner;

(I, src?GetS, E){
	src!Data<boolean exclusive = true>@resp;
	owner = src;
}

(I, src?GetM, M){
    src!Data<int[0..5] ackCount = sharers.count, boolean exclusive = false>@resp;
    owner = src;
}

(I, src?PutS){
    src!PutAck@fwd;
}

(I, src?PutM & src != owner){
    src!PutAck@fwd;
}

(I, src?PutE & src != owner){
    src!PutAck@fwd;
}



(S, src?GetS){
    src!Data<boolean exclusive = false>@resp;
    sharers.add(src);
}

(S, src?GetM & sharers.contains(src), M){
    sharers.del(src);
    src!Data<int[0..5] ackCount = sharers.count, boolean exclusive = false>@resp;
    sharers!Inv< cache requestor = src>@fwd;
    clear sharers;
    owner = src;
}

(S, src?GetM & !sharers.contains(src), M){
    src!Data<int[0..5] ackCount = sharers.count, boolean exclusive = false>@resp;
    sharers!Inv<cache requestor = src>@fwd; // caches need to know
                                            // where to send InvAck
    clear sharers;
    owner = src;
}


// need to check if the src is in the sharers. If it isn't we don't invalidate

(S, src?PutS & ( (sharers.count != 1) | (!sharers.contains(src)) )  ){
    sharers.del(src);
    src!PutAck@fwd;
}

// if this was the last sharer, invalidate
(S, src?PutS & sharers.count == 1 & sharers.contains(src), I){
    sharers.del(src);
    src!PutAck@fwd;
}


(S, src?PutM & src != owner){
    sharers.del(src);
    src!PutAck@fwd;
}

(S, src?PutE & src != owner){
    sharers.del(src);
    src!PutAck@fwd;
}





(M, src?GetS, S_D){
    owner!FwdGetS<cache requestor = src>@fwd;
    sharers.add(src);
    sharers.add(owner);
    clear owner;
}



(M, src?GetM){
    owner!FwdGetM<cache requestor = src>@fwd;
    owner = src;
}


(M, src?PutS){
    src!PutAck@fwd;
}


(M, src?PutM & src == owner, I){
    clear owner;
    src!PutAck@fwd;
}

(M, src?PutM & src!=owner){
    src!PutAck@fwd;
}

(M, src?PutE & src!=owner){
    src!PutAck@fwd;
}




(E, src?GetS, S_D){
    owner!FwdGetS<cache requestor = src>@fwd;
    sharers.add(src);
    sharers.add(owner);
    clear owner;
}

(E, src?GetM, M){
    owner!FwdGetM<cache requestor = src>@fwd;
    owner = src;
}

(E, src?PutS){
    src!PutAck@fwd;
}

(E, src?PutM & src == owner, I){
    clear owner;
    src!PutAck@fwd;
}

(E, src?PutM & src!=owner){
    src!PutAck@fwd;
}

(E, src?PutE & src == owner, I){
    clear owner;
    src!PutAck@fwd;
}

(E, src?PutE & src!=owner){
    src!PutAck@fwd;
}




(S_D, src?GetS){
    stall;
}

(S_D, src?GetM){
    stall;
}

(S_D, src?PutS){
    sharers.del(src);
    src!PutAck@fwd;
}

(S_D, src?PutM & src!=owner){
    sharers.del(src);
    src!PutAck@fwd;
}

(S_D, src?PutE & src!=owner){
    sharers.del(src);
    src!PutAck@fwd;
}

(S_D, src?Data, S){

}



// directory declaration ends here


}
