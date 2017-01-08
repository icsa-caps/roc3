// this is an MI protocol

networks: unordered {ch1}; // we need just one channel

machine Cache[4] {

startstate: I;

	(I, *Read_Write, IM) {
		Dir[0]!GetM@ch1;
	}

	(IM, *Read_Write) {
		stall;
	}


	(IM, src?Data, M) {
		Dir[0]!Ack@ch1;
	}

	(M, *Read_Write) {
  		hit;
	}

	(M, src?Fwd<Cache toSend>, I) {
		toSend!Data@ch1;
	}

}

nonsymmetric machine Dir[1] {

startstate: I;

Cache owner;


	(I, src?GetM, IM) {
		owner = src;
		src!Data@ch1;
	}

	(IM, src?GetM){
		stall;
	}

	(IM, src?Ack, M) {
	}

	(M, src?GetM, IM) {
		owner!Fwd<Cache toSend = src>@ch1;
		owner = src;
	}
}