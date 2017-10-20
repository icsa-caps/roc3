// MSI Snooping protocol from Sorin/Hill/Wood

// Networks
networks: ordered reqNet {req},
	  ordered resNet {res};


// Caches
machine cache[2] {

// Init
startstate: I;

// Vars

// Processor core events
(I, *load, IS_AD){
    all!GetS@req;
}
(I, *store, IM_AD){
    all!GetM@req;
}
(S, *store, SM_AD){
    all!GetM@req;
}
(S, *evict, I){
}
(M, *evict, MI_A){
    all!PutM@req;    
}

// Bus events (own transaction)
(IS_AD, self?GetS, IS_D){
}
(IS_AD, self?GetM, IS_D){
}
(IS_AD, src?Data, IS_A){
}
(IS_D, src?Data,S){
}
(IS_A, self?GetS, S){
}
(IS_A, self?GetM, S){
}
(IM_AD, self?GetM, IM_D){
}
(IM_AD, src?Data, IM_A){
}
(IM_D, src?Data, M){
}
(IM_A, self?GetM, M){
}
(SM_AD, self?GetM, SM_D){
}
(SM_AD, src?Data, SM_A){
}
(SM_D, src?Data, M){
}
(SM_A, self?GetM, M){
}
(MI_A, self?PutM, I){
    mem[0]!Data@res;
}
(II_A, self?PutM, I){
}

// Bus events (other's transaction)
(I, src?GetS & src!=self){
}
(I, src?GetM & src!=self){
}
(I, src?PutM & src!=self){
}
(IS_AD, src?GetS & src!=self){
}
(IS_AD, src?GetM & src!=self){
}
(IS_AD, src?PutM & src!=self){
}
(IS_D, src?GetS & src!=self){
}
(IS_D, src?GetM & src!=self){
    stall;
}
(IS_A, src?GetS & src!=self){
}
(IS_A, src?GetM & src!=self){
}
(IM_AD, src?GetS & src!=self){
}
(IM_AD, src?GetM & src!=self){
}
(IM_AD, src?PutM & src!=self){
}
(IM_D, src?GetS & src!=self){
    stall;
}
(IM_D, src?GetM & src!=self){
    stall;
}
(IM_A, src?GetS & src!=self){
}
(IM_A, src?GetM & src!=self){
}
(S, src?GetS & src!=self){
}
(S, src?GetM & src!=self, I){
}
(SM_AD, src?GetS & src!=self){
}
(SM_AD, src?GetM & src!=self, IM_AD){
}
(SM_D, src?GetS & src!=self){
    stall;
}
(SM_D, src?GetM & src!=self){
    stall;
}
(SM_A, src?GetS & src!=self){
}
(SM_A, src?GetM & src!=self, IM_A){
}
(M, src?GetS & src!=self, S){
    src!Data@res;
    mem[0]!Data@res;
}
(M, src?GetM & src!=self, I){
    src!Data@res;
}
(MI_A, src?GetS & src!=self, II_A){
    src!Data@res;
    mem[0]!Data@res;
}
(MI_A, src?GetM & src!=self, II_A){
    src!Data@res;
}
(II_A, src?GetS & src!=self){
}
(II_A, src?GetM & src!=self){
}
(II_A, src?PutM & src!=self){
}
}


// Memory controller
machine mem {

// Init
startstate: IorS;

// Vars
cache owner;

// Bus events
(IorS, src?GetS){
    src!Data@res;
}
(IorS, src?GetM, M){
    src!Data@res;
    owner = src;
}
(IorS, src?PutM & src!=owner){
}
(IorS_D, src?GetS){
    stall;
}
(IorS_D, src?GetM){
    stall;
}
(IorS_D, src?PutM & src==owner){
    stall;
}
(IorS_D, src?PutM & src!=owner){
}
(IorS_D, src?Data, IorS){
}
(IorS_A, src?GetS, IorS){
    clear owner;
}
(IorS_A, src?GetM){
}
(IorS_A, src?PutM & src==owner){
    clear owner;
}
(IorS_A, src?PutM & src!=owner){
}
(M, src?GetS, IorS_D){
    clear owner;
}
(M, src?GetM){
    owner = src;
}
(M, src?PutM & src==owner, IorS_D){
    clear owner;
}
(M, src?PutM & src!=owner, IorS_D){
}
(M, src?Data, IorS_A){
}
}