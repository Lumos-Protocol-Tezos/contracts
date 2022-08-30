module InvestmentToken = struct 
  type token_id = nat 
  type token_address = address
  type token_type = string (* TODO *)

  type token_details = {
    token_address : token_address;
    token_type : token_type;
    decimals : nat;
    is_interest_bearing : bool; 
  }

  type t = (token_id, token_details) big_map
end

module FA12Token = struct 
  type transfer = [@layout:comb] {
    from: address;
    to : address;
    value: nat
  }

  let get_transfer_ep (addr : InvestmentToken.token_address) : transfer contract = 
    match (Tezos.get_entrypoint_opt ("%transfer") (addr : address) : transfer contract option) with
    | None -> (failwith "TRANSFER_EP_NOT_FOUND")
    | Some ep -> ep
end

module NftToken = struct 
  type token_id = nat 
  type token_address = address

  type balance_of_request = [@layout:comb]{
    owner : address;
    token_id : nat;
  }

  type balance_of_response = [@layout:comb]{
    request : balance_of_request;
    balance : nat;
  }

  type balance_of = [@layout:comb]{
  requests : balance_of_request list;
  callback : (balance_of_response list) contract;
  }
end

module SmartWallet = struct 
  type sw_address = address
  type wm_address = address 
  type storage = {
    wallet_manager : wm_address;
    nft_address : NftToken.token_address * NftToken.token_id;
  }
  type create_param = [@layout:comb]{ 
    delegate : key_hash option;
    balance : tez;
    storage : storage 
  }
  type create_result =[@layout:comb]{
    create_op: operation;
    addr : address 
  }

  type withdraw_fa12 = {
    receiver_address : address;
    invst_token_address : InvestmentToken.token_address;
    amount : nat;
  }
  
  type withdraw_interest_bearing_fa12 = {
    yupana_token_id : nat;
    receiver_address : address;
    invst_token_address : InvestmentToken.token_address;
    amount : nat;
  }

  type invest_interest_bearing_fa12 = {
    yupana_token_id: nat;
    invst_token_address : address;
    amount : nat;
  }

  type invest_callback = {
    invest_param : invest_interest_bearing_fa12;
    sw_address : address;
  }

  let get_invest_ep (sw_addr : address) : invest_interest_bearing_fa12 contract = 
    match (Tezos.get_entrypoint_opt ("%investInterestBearingFa12") sw_addr: invest_interest_bearing_fa12 contract option) with
    | None -> (failwith "INVEST_EP_NOT_FOUND")
    | Some ep -> ep
end

module Storage = struct 
  type withdraw_fa12_param = {
    nft_address : NftToken.token_address;
    nft_id : NftToken.token_id;
    invst_token_address : InvestmentToken.token_address;
    receiver_address : address;
    withdrawer : address; (* not needed *)
    amount : nat;
  }
  
  type withdraw_interest_bearing_fa12_param = {
    nft_address : NftToken.token_address;
    nft_id : NftToken.token_id;
    invst_token_address : InvestmentToken.token_address;
    yupana_token_id : nat;
    receiver_address : address;
    withdrawer : address; (* not needed *)
    amount : nat;
  }

  type t = {
    owner_wallet_map : (NftToken.token_address * NftToken.token_id, SmartWallet.sw_address) big_map;
    recent_balance_requests : (address * nat, address) big_map; (* owner_address * token_id, token_address *)
    withdraw_requests : (address * nat, withdraw_fa12_param) big_map;
    withdraw_interest_bearing_requests : (address * nat, withdraw_interest_bearing_fa12_param) big_map;
  }

  type transfer_details = {
    invst_tkn_transfer_ep : FA12Token.transfer contract;
    value : nat;
  }
end

type create_and_transfer_smart_wallet_param = {
  create_contract : SmartWallet.create_param;
  transfer : Storage.transfer_details;
  nft_address : NftToken.token_address;
  nft_id : NftToken.token_id;
}

type create_and_invest_smart_wallet_param = {
  create_contract : SmartWallet.create_param;
  transfer : Storage.transfer_details;
  yupana_token_id : nat;
  invst_token_address : InvestmentToken.token_address;
  nft_address : NftToken.token_address;
  nft_id : NftToken.token_id;
}

type get_wallet_address_param = {
  nft_address: NftToken.token_address;
  nft_id : NftToken.token_id;
}

type balance_of_query_param = {
  requests : NftToken.balance_of_request list;
  nft_address : address;
  withdraw_fa12 : Storage.withdraw_fa12_param;
}

type balance_of_query_yup_param = {
  requests : NftToken.balance_of_request list;
  nft_address : address;
  withdraw_interest_bearing_fa12 : Storage.withdraw_interest_bearing_fa12_param;
}
type balance_of_query_yup_param = {
  requests : NftToken.balance_of_request list;
  nft_address : address;
  withdraw_interest_bearing_fa12 : Storage.withdraw_interest_bearing_fa12_param;
}

type return = (operation list * Storage.t)

type parameter = 
  | WithdrawFa12 of Storage.withdraw_fa12_param
  | WithdrawInterestBearingFa12 of Storage.withdraw_interest_bearing_fa12_param
  | BalanceOfQuery of balance_of_query_param
  | BalanceOfQueryYup of balance_of_query_yup_param
  | BalanceOfResponse of NftToken.balance_of_response list
  | BalanceOfResponseYup of NftToken.balance_of_response list
  | CreateAndInvestSmartWallet of create_and_invest_smart_wallet_param
  | InvestCallback of SmartWallet.invest_callback 
  | CreateAndTransferSmartWallet of create_and_transfer_smart_wallet_param 

[@view]
let get_wallet_address (p, s : (get_wallet_address_param * Storage.t)) : SmartWallet.sw_address option = 
  let addr : SmartWallet.sw_address option = 
    Big_map.find_opt (p.nft_address, p.nft_id) s.owner_wallet_map in
  addr 

let balance_of_query (p,s : (balance_of_query_param * Storage.t)) : return = 
  let slf_address : address = Tezos.get_self_address() in
  let cb_opt : NftToken.balance_of_response list contract option = 
    Tezos.get_entrypoint_opt "%balanceOfResponse" slf_address in
  let cb : NftToken.balance_of_response list contract = match cb_opt with
    | None -> (failwith "NO_RESPONSE_ENTRYPOINT" : NftToken.balance_of_response list contract)
    | Some ep -> ep in
  let bp : NftToken.balance_of = {
    requests = p.requests;
    callback = cb;
  } in
  let fa2 : NftToken.balance_of contract option = 
    Tezos.get_entrypoint_opt "%balance_of" p.nft_address in
  let q_op = match fa2 with
    | None -> (failwith "NO_BALANCE_OF_ENTRYPOINT" : operation)
    | Some ep -> Tezos.transaction bp 0mutez ep in
  let new_recent_balance_requests = 
    Big_map.update (p.withdraw_fa12.withdrawer, (p.withdraw_fa12.nft_id: nat)) (Some p.nft_address) s.recent_balance_requests in
  let new_withdraw_requests = 
    Big_map.update (p.withdraw_fa12.withdrawer, (p.withdraw_fa12.nft_id: nat)) (Some p.withdraw_fa12) s.withdraw_requests in
  ([q_op], { s with recent_balance_requests = new_recent_balance_requests; withdraw_requests = new_withdraw_requests })

(* for withdrawal of y token - move to a diff file later *)
let balance_of_query_yup (p,s : (balance_of_query_yup_param * Storage.t)) : return = 
  let slf_address : address = Tezos.get_self_address() in
  let cb_opt : NftToken.balance_of_response list contract option = 
    Tezos.get_entrypoint_opt "%balanceOfResponseYup" slf_address in
  let cb : NftToken.balance_of_response list contract = match cb_opt with
    | None -> (failwith "NO_RESPONSE_ENTRYPOINT" : NftToken.balance_of_response list contract)
    | Some ep -> ep in
  let bp : NftToken.balance_of = {
    requests = p.requests;
    callback = cb;
  } in
  let fa2 : NftToken.balance_of contract option = 
    Tezos.get_entrypoint_opt "%balance_of" p.nft_address in
  let q_op = match fa2 with
    | None -> (failwith "NO_BALANCE_OF_ENTRYPOINT" : operation)
    | Some ep -> Tezos.transaction bp 0mutez ep in
  let new_recent_balance_requests = 
    Big_map.update (p.withdraw_interest_bearing_fa12.withdrawer, (p.withdraw_interest_bearing_fa12.nft_id: nat)) (Some p.nft_address) s.recent_balance_requests in
  let new_withdraw_requests = 
    Big_map.update (p.withdraw_interest_bearing_fa12.withdrawer, (p.withdraw_interest_bearing_fa12.nft_id: nat)) (Some p.withdraw_interest_bearing_fa12) s.withdraw_interest_bearing_requests in
  ([q_op], { s with recent_balance_requests = new_recent_balance_requests; withdraw_interest_bearing_requests = new_withdraw_requests})

let balance_of_response (p,s : (NftToken.balance_of_response list * Storage.t)) : return = 
    (* TODO for validation of sender *)
    let bal_res: NftToken.balance_of_response = match p with 
    | []  -> (failwith "INVALID_BAL" : NftToken.balance_of_response)
    | x :: _xs -> x in
    if bal_res.balance = 0n then failwith "ONLY_OWNER_CAN_TRANSFER" else
    let token_address : address = 
      match (Big_map.find_opt (bal_res.request.owner, bal_res.request.token_id) s.recent_balance_requests) with 
      | None -> (failwith "NO_BALANCE_REQ_FOUND" : address)
      | Some addr -> addr in 
    if Tezos.get_sender() <> token_address then failwith "INCORRECT_TOKEN" else
    let withdraw_param : Storage.withdraw_fa12_param = match (Big_map.find_opt (bal_res.request.owner, bal_res.request.token_id) s.withdraw_requests) with
      | None -> (failwith "NO_WITHDRAW_REQ_FOUND" : Storage.withdraw_fa12_param)
      | Some wp -> wp in
    let transfer_tr_self = Tezos.transaction withdraw_param 0tez (Tezos.self "%withdrawFa12" : Storage.withdraw_fa12_param contract) in
    ([transfer_tr_self], s )

let balance_of_response_yup (p,s : (NftToken.balance_of_response list * Storage.t)) : return = 
    (* TODO for validation of sender *)
    let bal_res: NftToken.balance_of_response = match p with 
    | []  -> (failwith "INVALID_BAL" : NftToken.balance_of_response)
    | x :: _xs -> x in
    if bal_res.balance = 0n then failwith "ONLY_OWNER_CAN_TRANSFER" else
    let token_address : address = 
      match (Big_map.find_opt (bal_res.request.owner, bal_res.request.token_id) s.recent_balance_requests) with 
      | None -> (failwith "NO_BALANCE_REQ_FOUND" : address)
      | Some addr -> addr in 
    if Tezos.get_sender() <> token_address then failwith "INCORRECT_TOKEN" else
    let withdraw_param : Storage.withdraw_interest_bearing_fa12_param = 
      match (Big_map.find_opt (bal_res.request.owner, bal_res.request.token_id) s.withdraw_interest_bearing_requests) with
      | None -> (failwith "NO_WITHDRAW_REQ_FOUND" : Storage.withdraw_interest_bearing_fa12_param)
      | Some wp -> wp in
    let transfer_tr_self = 
      Tezos.transaction withdraw_param 0tez 
        (Tezos.self "%withdrawInterestBearingFa12" : Storage.withdraw_interest_bearing_fa12_param contract) in
    ([transfer_tr_self], s )

let withdraw_fa12 (p,s : (Storage.withdraw_fa12_param * Storage.t)) : return = 
  let () = assert_with_error (Tezos.get_sender() = Tezos.get_self_address()) ("ONLY_SELF_ALLOWED") in
  let smart_wallet_address : SmartWallet.sw_address = match (Big_map.find_opt (p.nft_address, p.nft_id) s.owner_wallet_map) with 
    | None -> (failwith "WALLET_NOT_FOUND" : SmartWallet.sw_address) 
    | Some wlt -> wlt  in 
  let sw_withdraw_ep: SmartWallet.withdraw_fa12 contract = match (Tezos.get_entrypoint_opt "%withdrawFa12" smart_wallet_address : SmartWallet.withdraw_fa12 contract option) with 
    | None -> (failwith "WALLET_CONTRACT_NOT_FOUND": SmartWallet.withdraw_fa12 contract)
    | Some ctr -> ctr in 
  let withdraw : SmartWallet.withdraw_fa12 = {
    receiver_address = p.withdrawer;
    invst_token_address = p.invst_token_address;
    amount = p.amount;
  } in
  let tr : operation = Tezos.transaction withdraw 0tez sw_withdraw_ep in
  ([tr], s)

let withdraw_interest_bearing_fa12 (p,s : (Storage.withdraw_interest_bearing_fa12_param * Storage.t)) : return = 
  let () = assert_with_error (Tezos.get_sender() = Tezos.get_self_address()) ("ONLY_SELF_ALLOWED") in
  let smart_wallet_address : SmartWallet.sw_address = match (Big_map.find_opt (p.nft_address, p.nft_id) s.owner_wallet_map) with 
    | None -> (failwith "WALLET_NOT_FOUND" : SmartWallet.sw_address) 
    | Some wlt -> wlt  in 
  let sw_withdraw_ep: SmartWallet.withdraw_interest_bearing_fa12 contract = 
    match (Tezos.get_entrypoint_opt "%withdrawInterestBearingFa12" smart_wallet_address : SmartWallet.withdraw_interest_bearing_fa12 contract option) with 
    | None -> (failwith "WALLET_CONTRACT_NOT_FOUND": SmartWallet.withdraw_interest_bearing_fa12 contract)
    | Some ctr -> ctr in 
  let withdraw : SmartWallet.withdraw_interest_bearing_fa12 = {
    yupana_token_id = p.yupana_token_id;
    receiver_address = p.withdrawer;
    invst_token_address = p.invst_token_address;
    amount = p.amount;
  } in
  let tr : operation = Tezos.transaction withdraw 0tez sw_withdraw_ep in
  ([tr], s)

[@inline]
  let create_smart_wallet = 
    [%Michelson ({|{ 
UNPAIR 3;
CREATE_CONTRACT
{ parameter
    (or (or (pair %investInterestBearingFa12
               (pair (nat %amount) (address %invst_token_address))
               (nat %yupana_token_id))
            (pair %investInterestBearingFa12Step2
               (pair (nat %amount) (address %invst_token_address))
               (nat %yupana_token_id)))
        (pair %withdrawFa12
           (pair (nat %amount) (address %invst_token_address))
           (address %receiver_address))) ;
  storage (pair (pair %nft_address address nat) (address %wallet_manager)) ;
  code { UNPAIR ;
         IF_LEFT
           { IF_LEFT
               { DUP ;
                 CAR ;
                 CDR ;
                 CONTRACT %approve (pair (address %spender) (nat %value)) ;
                 IF_NONE { PUSH string "ALLOW_EP_NOT_FOUND" ; FAILWITH } {} ;
                 PUSH mutez 0 ;
                 DUP 3 ;
                 CAR ;
                 CAR ;
                 PUSH address "KT1PW3aKxfB89HUrq8ywnw9tLvxtuHLgsjJW" ;
                 PAIR ;
                 TRANSFER_TOKENS ;
                 PUSH address "KT1MZeSimmt1A3omsJXjKy9ihma1ajUPqD4m" ;
                 CONTRACT %getPrice (set nat) ;
                 IF_NONE { PUSH string "GET_PRICE_EP_NOT_FOUND" ; FAILWITH } {} ;
                 PUSH mutez 0 ;
                 EMPTY_SET nat ;
                 DUP 5 ;
                 CDR ;
                 PUSH bool True ;
                 SWAP ;
                 UPDATE ;
                 TRANSFER_TOKENS ;
                 PUSH address "KT1PW3aKxfB89HUrq8ywnw9tLvxtuHLgsjJW" ;
                 CONTRACT %updateInterest nat ;
                 IF_NONE { PUSH string "UPDATE_INTEREST_EP_NOT_FOUND" ; FAILWITH } {} ;
                 PUSH mutez 0 ;
                 DUP 5 ;
                 CDR ;
                 TRANSFER_TOKENS ;
                 SELF %investInterestBearingFa12Step2 ;
                 PUSH mutez 0 ;
                 DIG 5 ;
                 PAIR 3 ;
                 UNPAIR 3 ;
                 TRANSFER_TOKENS ;
                 DIG 4 ;
                 NIL operation ;
                 DIG 2 ;
                 CONS ;
                 DIG 2 ;
                 CONS ;
                 DIG 2 ;
                 CONS }
               { PUSH address "KT1PW3aKxfB89HUrq8ywnw9tLvxtuHLgsjJW" ;
                 CONTRACT %mint (pair (nat %token_id) (nat %amount) (nat %min_received)) ;
                 IF_NONE { PUSH string "MINT_EP_NOT_FOUND" ; FAILWITH } {} ;
                 PUSH mutez 0 ;
                 PUSH nat 1 ;
                 DUP 4 ;
                 CAR ;
                 CAR ;
                 DIG 4 ;
                 CDR ;
                 PAIR 3 ;
                 TRANSFER_TOKENS ;
                 SWAP ;
                 NIL operation } ;
             DIG 2 ;
             CONS ;
             PAIR }
           { DUP 2 ;
             CDR ;
             SENDER ;
             COMPARE ;
             NEQ ;
             IF { DROP 2 ; PUSH string "ONLY_WALLET_MANAGER_ALLOWED" ; FAILWITH }
                { DUP ;
                  CAR ;
                  CDR ;
                  CONTRACT %transfer (pair (address %from) (address %to) (nat %value)) ;
                  IF_NONE { PUSH string "TRANSFER_EP_NOT_FOUND" ; FAILWITH } {} ;
                  DUP 2 ;
                  CAR ;
                  CAR ;
                  DIG 2 ;
                  CDR ;
                  SELF_ADDRESS ;
                  PAIR 3 ;
                  SWAP ;
                  PUSH mutez 0 ;
                  DIG 2 ;
                  TRANSFER_TOKENS ;
                  SWAP ;
                  NIL operation ;
                  DIG 2 ;
                  CONS ;
                  PAIR } } } };
      PAIR
    }|} : SmartWallet.create_param -> SmartWallet.create_result)]

let create_and_transfer_smart_wallet (p,s : (create_and_transfer_smart_wallet_param * Storage.t)) : return = 
  let res = create_smart_wallet p.create_contract in
  let transfer_param : FA12Token.transfer = {
    from = Tezos.get_self_address();
    to = res.addr;
    value = p.transfer.value;
  } in
  let transfer_op = Tezos.transaction transfer_param 0tez p.transfer.invst_tkn_transfer_ep in
  let new_wallet_map = Big_map.update (p.nft_address,p.nft_id) (Some res.addr : SmartWallet.sw_address option) s.owner_wallet_map in 
([res.create_op; transfer_op], {s with owner_wallet_map = new_wallet_map})


let create_and_invest_smart_wallet (p,s : (create_and_invest_smart_wallet_param * Storage.t)) : return = 
  let res = create_smart_wallet p.create_contract in 
  let transfer_param : FA12Token.transfer = {
    from = Tezos.get_self_address();
    to = res.addr;
    value = p.transfer.value;
  } in
  let transfer_op = Tezos.transaction transfer_param 0tez p.transfer.invst_tkn_transfer_ep in
  let new_wallet_map = Big_map.update (p.nft_address,p.nft_id) (Some res.addr : SmartWallet.sw_address option) s.owner_wallet_map in 
  let invest_param: SmartWallet.invest_interest_bearing_fa12 = {
    yupana_token_id = (p.yupana_token_id : nat);
    invst_token_address = (p.invst_token_address: address);
    amount = p.transfer.value; 
  } in
  let callback_param = {
    invest_param = invest_param;
    sw_address = res.addr;
  } in
  let invst_callback_tr = Tezos.transaction callback_param 0tez 
    (Tezos.self "%investCallback" : SmartWallet.invest_callback contract) in
([res.create_op; transfer_op; invst_callback_tr], {s with owner_wallet_map = new_wallet_map})

let invest_callback (p,s : (SmartWallet.invest_callback * Storage.t)) : return =
  let invest_ep = SmartWallet.get_invest_ep p.sw_address in
  let invest_tr = Tezos.transaction p.invest_param 0tez invest_ep in 
  ([invest_tr], s)

let main (param, storage: (parameter * Storage.t)) : return = 
  match param with 
  | WithdrawFa12 p -> withdraw_fa12 (p, storage)
  | WithdrawInterestBearingFa12 p -> withdraw_interest_bearing_fa12 (p, storage)
  | BalanceOfQuery p -> balance_of_query (p, storage)
  | BalanceOfQueryYup p -> balance_of_query_yup (p, storage)
  | CreateAndTransferSmartWallet p -> create_and_transfer_smart_wallet (p, storage) 
  | CreateAndInvestSmartWallet p -> create_and_invest_smart_wallet (p,storage)
  | InvestCallback p -> invest_callback (p, storage)
  | BalanceOfResponse p -> balance_of_response (p, storage)
  | BalanceOfResponseYup p -> balance_of_response_yup (p, storage)
