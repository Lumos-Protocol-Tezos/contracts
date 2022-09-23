module Yupana = struct 
  type token_id = nat
end

module InvestmentToken = struct 
  type token_id = nat 
  type token_address = address
  type token_type = string (* TODO *)

  type token_details = {
    token_address : token_address;
    token_type : token_type;
    decimals : nat;
    yupana_token_id : Yupana.token_id option; 
  }

  type t = (token_id, token_details) big_map
end

module NftToken = struct 
  type token_id = nat 
  type token_address = address
end

module SmartWallet = struct 
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
  type invest_interest_bearing_fa12 = {
    yupana_token_id: nat;
    invst_token_address : address;
    amount : nat;
  }

  let get_invest_ep (sw_addr : address) : invest_interest_bearing_fa12 contract = 
    match (Tezos.get_entrypoint_opt ("%investInterestBearingFa12") sw_addr : invest_interest_bearing_fa12 contract option) with
    | None -> (failwith "INVEST_EP_NOT_FOUND")
    | Some ep -> ep
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

module WalletManager = struct 
  type wm_address = address

  type smart_wallet_key = {
    nft_address : NftToken.token_address;
    nft_id : NftToken.token_id;
  }
  type transfer_details = {
    invst_tkn_transfer_ep : FA12Token.transfer contract;
    value : nat;
  }
 
  type create_and_transfer_smart_wallet = {
    create_contract : SmartWallet.create_param;
    transfer : transfer_details;
    nft_address : NftToken.token_address;
    nft_id : NftToken.token_id;
  }

  type create_and_invest_smart_wallet = {
    create_contract : SmartWallet.create_param;
    transfer : transfer_details;
    yupana_token_id : nat;
    invst_token_address : InvestmentToken.token_address;
    nft_address : NftToken.token_address;
    nft_id : NftToken.token_id;
  }

  type withdraw_fa12 = {
    nft_address : NftToken.token_address;
    nft_id : NftToken.token_id;
    invst_token_address : InvestmentToken.token_address;
    receiver_address : address;
    withdrawer : address;
    amount : nat;
  }
  
  type withdraw_interest_bearing_fa12 = {
    nft_address : NftToken.token_address;
    nft_id : NftToken.token_id;
    invst_token_address : InvestmentToken.token_address;
    yupana_token_id : nat;
    receiver_address : address;
    withdrawer : address;
    amount : nat;
  
  }
  type balance_of_request = [@layout:comb]{
    owner : address;
    token_id : nat;
  }
  type balance_of_query = {
    requests : balance_of_request list;
    nft_address : address;
    withdraw_fa12 : withdraw_fa12;
  } 

  type balance_of_query_yup = {
    requests : balance_of_request list;
    nft_address : address;
    withdraw_interest_bearing_fa12 : withdraw_interest_bearing_fa12;
  }

  let get_smart_wallet_addr (k, wm_addr: smart_wallet_key * wm_address) : address option = 
    Tezos.call_view "get_wallet_address" k (wm_addr: address) 

  let get_create_and_transfer_wallet_ep  (wm_addr : wm_address ) : create_and_transfer_smart_wallet contract = 
    match (Tezos.get_entrypoint_opt ("%createAndTransferSmartWallet") wm_addr: create_and_transfer_smart_wallet contract option) with 
      | None -> (failwith "CREATE_SMART_WALLET_EP_NOT_FOUND" : create_and_transfer_smart_wallet contract)
      | Some ep -> ep

  let get_create_and_invest_wallet_ep (wm_addr : wm_address) : create_and_invest_smart_wallet contract =
    match (Tezos.get_entrypoint_opt ("%createAndInvestSmartWallet") wm_addr : create_and_invest_smart_wallet contract option) with
     | None -> (failwith "CREATE_SMART_WALLET_EP_NOT_FOUND")
     | Some ep -> ep

  let get_bal_of_ep (wm_addr : wm_address): balance_of_query contract =
    match (Tezos.get_entrypoint_opt "%balanceOfQuery" wm_addr : balance_of_query contract option) with 
    | None -> (failwith "WM_BALANCE_OF_ENTRYPOINT_NOT_FOUND" : balance_of_query contract)
    | Some ctr -> ctr 
  
  let get_bal_of_yup_ep (wm_addr : wm_address) : balance_of_query_yup contract = 
    match (Tezos.get_entrypoint_opt "%balanceOfQueryYup" wm_addr : balance_of_query_yup contract option) with 
    | None -> (failwith "WM_BALANCE_OF_YUP_ENTRYPOINT_NOT_FOUND" : balance_of_query_yup contract)
    | Some ctr -> ctr 

end

module Storage = struct 
  type t = {
    wm_map : (InvestmentToken.token_id, WalletManager.wm_address) big_map; 
    invst_tokens : (InvestmentToken.token_id, InvestmentToken.token_details) big_map;
    admin : address;
  }
end

type add_wallet_manager_param = {
  token_id : InvestmentToken.token_id;
  token_details : InvestmentToken.token_details;
  wm_address: WalletManager.wm_address;
}

type illuminate_param = {
  nft_address : NftToken.token_address ;
  nft_id : NftToken.token_id;
  token_id : InvestmentToken.token_id;
  amount : nat;
  (*settings for illumination*)
}

type illuminate_with_interest_param = {
  nft_address : NftToken.token_address;
  nft_id : NftToken.token_id;
  token_id : InvestmentToken.token_id;
  amount : nat;
}

type withdraw_param = {
  nft_address : NftToken.token_address;
  nft_id : NftToken.token_id;
  invst_token_id : InvestmentToken.token_id; 
  receiver_address : address;
  amount : nat;
}

type parameter = 
  | Illuminate of illuminate_param
  | IlluminateWithInterest of illuminate_with_interest_param
  | WithdrawFa12 of withdraw_param
  | AddWalletManager of add_wallet_manager_param

type return = (operation list * Storage.t)

let add_wallet_manager (p, s : add_wallet_manager_param * Storage.t) : return =
  let () = assert_with_error (Tezos.get_sender() = s.admin) ("ONLY_ADMIN_ALLOWED") in
  let new_invst_tokens = Big_map.update p.token_id (Some p.token_details) s.invst_tokens in
  let new_wm_map = Big_map.update p.token_id (Some p.wm_address) s.wm_map in
  ([], {s with wm_map = new_wm_map; invst_tokens = new_invst_tokens;})

(* TODO  when illuminated, fetch settings if exists else create settings *)
let illuminate (p,s : illuminate_param * Storage.t) : return = 
  let wm_addr_opt = Big_map.find_opt p.token_id s.wm_map in
  let wm_addr = match wm_addr_opt with 
    | None -> (failwith "WM_NOT_FOUND") 
    | Some addr -> addr in
  let invst_tkn_details_opt = Big_map.find_opt p.token_id s.invst_tokens in
  let invst_token_details = match invst_tkn_details_opt with 
    | None -> (failwith "INVST_TOKEN_NOT_FOUND") 
    | Some dts -> dts in 
  let invst_tkn_transfer_ep = FA12Token.get_transfer_ep invst_token_details.token_address in
  let transfer_self_param : FA12Token.transfer = {
    from = Tezos.get_sender();
    to = Tezos.get_self_address();
    value = p.amount; (* TODO add fees here*)
  } in
  let _transfer_to_self_txn : operation = (* TODO will be useful for fees *)
    Tezos.transaction transfer_self_param 0tez invst_tkn_transfer_ep in
  let smart_wallet_addr_key : WalletManager.smart_wallet_key = {
    nft_address = p.nft_address; 
    nft_id = p.nft_id;
  } in 
  let smart_wallet_addr_opt = WalletManager.get_smart_wallet_addr (smart_wallet_addr_key, wm_addr) in 
  let return : return = match smart_wallet_addr_opt with 
    | None -> 
      let transfer_to_mgr_param : FA12Token.transfer = {
        from = Tezos.get_sender();
        to = (wm_addr: address);
        value = p.amount; 
      } in 
      let transfer_to_mgr_tr : operation = 
        Tezos.transaction transfer_to_mgr_param 0tez invst_tkn_transfer_ep in 
      let init_storage : SmartWallet.storage = {
        wallet_manager = wm_addr;
        nft_address = (p.nft_address, p.nft_id);
      } in 
      let create_wallet_param : SmartWallet.create_param = {
        delegate = (None : key_hash option) ;
        balance = 0tez;
        storage = init_storage; 
      } in 
      let transfer_details : WalletManager.transfer_details = {
        invst_tkn_transfer_ep = invst_tkn_transfer_ep;
        value = p.amount;
      } in
      let create_and_transfer_smart_wallet_param : WalletManager.create_and_transfer_smart_wallet = {
        create_contract = create_wallet_param;
        transfer = transfer_details;
        nft_address = p.nft_address ;
        nft_id = p.nft_id;
      } in
      let create_and_transfer_contract_ep = WalletManager.get_create_and_transfer_wallet_ep wm_addr in
      let create_wallet_and_call_tr : operation = 
        Tezos.transaction create_and_transfer_smart_wallet_param 0tez create_and_transfer_contract_ep in
      ([transfer_to_mgr_tr; create_wallet_and_call_tr], s) 
    | Some addr -> 
        let transfer_to_wallet : FA12Token.transfer = {
          from = Tezos.get_sender();
          to = (addr: address);
          value = p.amount; 
        } in
        let transfer_to_wallet_tx : operation = 
          Tezos.transaction transfer_to_wallet 0tez invst_tkn_transfer_ep in
        ([transfer_to_wallet_tx],s)
  in return

let withdraw (p,s : withdraw_param * Storage.t) : return =
  let wm_addr_opt = Big_map.find_opt p.invst_token_id s.wm_map in
  let wm_addr = match wm_addr_opt with 
    | None -> (failwith "WM_NOT_FOUND") 
    | Some addr -> addr in
  let invst_tkn_details_opt = Big_map.find_opt p.invst_token_id s.invst_tokens in
  let invst_tkn_details = match invst_tkn_details_opt with
    | None -> (failwith "WM_NOT_FOUND")
    | Some dts -> dts in
  let withdraw_fa12_param : WalletManager.withdraw_fa12 = {
    nft_address = p.nft_address;
    nft_id = p.nft_id;
    invst_token_address = invst_tkn_details.token_address;
    receiver_address = p.receiver_address;
    withdrawer = Tezos.get_sender();
    amount = p.amount;
  } in 
  let balance_of_query_requests : WalletManager.balance_of_request list = [{
    owner = Tezos.get_sender();
    token_id = (p.nft_id : nat);
  }] in  
  let balance_of_query : WalletManager.balance_of_query = {
    requests = balance_of_query_requests;
    nft_address = (p.nft_address : address) ;
    withdraw_fa12 = withdraw_fa12_param;
  } in
  let bal_of_ep = WalletManager.get_bal_of_ep wm_addr in
  let bal_of_tr : operation = Tezos.transaction balance_of_query 0tez bal_of_ep in 
  ([bal_of_tr], s)

let withdraw_interest_bearing (p,s : withdraw_param * Storage.t) =
  let wm_addr_opt = Big_map.find_opt p.invst_token_id s.wm_map in
  let wm_addr = match wm_addr_opt with 
    | None -> (failwith "WM_NOT_FOUND") 
    | Some addr -> addr in
  let invst_tkn_details_opt = Big_map.find_opt p.invst_token_id s.invst_tokens in
  let invst_tkn_details = match invst_tkn_details_opt with
    | None -> (failwith "WM_NOT_FOUND")
    | Some dts -> dts in 
  let yupana_token_id : Yupana.token_id = match invst_tkn_details.yupana_token_id with
    | None -> (failwith "TOKEN_CANNOT_BE_ENERGIZED")
    | Some id -> id in
  let withdraw_fa12_param : WalletManager.withdraw_interest_bearing_fa12 = {
    nft_address = p.nft_address;
    nft_id = p.nft_id;
    invst_token_address = invst_tkn_details.token_address;
    receiver_address = p.receiver_address;
    yupana_token_id = (yupana_token_id : nat);
    withdrawer = Tezos.get_sender();
    amount = p.amount;
  } in 
  let balance_of_query_requests : WalletManager.balance_of_request list = [{
    owner = Tezos.get_sender();
    token_id = (p.nft_id : nat);
  }] in  
  let balance_of_query : WalletManager.balance_of_query_yup = {
    requests = balance_of_query_requests;
    nft_address = (p.nft_address : address) ;
    withdraw_interest_bearing_fa12 = withdraw_fa12_param;
  } in 
  let bal_of_yup_ep = WalletManager.get_bal_of_yup_ep wm_addr in
  let bal_of_tr : operation = Tezos.transaction balance_of_query 0tez bal_of_yup_ep in 
  ([bal_of_tr], s)

let illuminate_with_interest (p,s :illuminate_with_interest_param * Storage.t) = 
  let wm_addr_opt = Big_map.find_opt p.token_id s.wm_map in
  let wm_addr = match wm_addr_opt with 
    | None -> (failwith "WM_NOT_FOUND") 
    | Some addr -> addr in
  let invst_tkn_details_opt = Big_map.find_opt p.token_id s.invst_tokens in
  let invst_token_details = match invst_tkn_details_opt with 
    | None -> (failwith "INVST_TOKEN_NOT_FOUND") 
    | Some dts -> dts in 
  let yupana_token_id : Yupana.token_id = match invst_token_details.yupana_token_id with
    | None -> (failwith "TOKEN_CANNOT_BE_ENERGIZED")
    | Some id -> id in
  let invst_tkn_transfer_ep = FA12Token.get_transfer_ep invst_token_details.token_address in
  let transfer_self_param : FA12Token.transfer = {
    from = Tezos.get_sender();
    to = Tezos.get_self_address();
    value = p.amount; (* TODO add fees here*)
  } in
  let _transfer_to_self_txn : operation = (* TODO will be useful for fees *)
    Tezos.transaction transfer_self_param 0tez invst_tkn_transfer_ep in
  let smart_wallet_addr_key : WalletManager.smart_wallet_key = {
    nft_address = p.nft_address; 
    nft_id = p.nft_id;
  } in 
  let smart_wallet_addr_opt = WalletManager.get_smart_wallet_addr (smart_wallet_addr_key, wm_addr) in 
  let return : return = match smart_wallet_addr_opt with 
    | None -> 
      let transfer_to_mgr_param : FA12Token.transfer = {
        from = Tezos.get_sender();
        to = (wm_addr: address);
        value = p.amount; 
      } in 
      let transfer_to_mgr_tr : operation = 
        Tezos.transaction transfer_to_mgr_param 0tez invst_tkn_transfer_ep in 
      let init_storage : SmartWallet.storage = {
        wallet_manager = wm_addr;
        nft_address = (p.nft_address, p.nft_id);
      } in 
      let create_wallet_param : SmartWallet.create_param = {
        delegate = (None : key_hash option) ;
        balance = 0tez;
        storage = init_storage; 
      } in 
      let transfer_details : WalletManager.transfer_details = {
        invst_tkn_transfer_ep = invst_tkn_transfer_ep;
        value = p.amount;
      } in
      let create_and_invest_smart_wallet_param : WalletManager.create_and_invest_smart_wallet = {
        create_contract = create_wallet_param;
        transfer = transfer_details;
        yupana_token_id = yupana_token_id;
        invst_token_address = invst_token_details.token_address; 
        nft_address = p.nft_address;
        nft_id = p.nft_id;
      } in
      let create_and_invest_contract_ep = WalletManager.get_create_and_invest_wallet_ep wm_addr in
      let create_wallet_and_invest_tr : operation = 
        Tezos.transaction create_and_invest_smart_wallet_param 0tez create_and_invest_contract_ep in
      ([transfer_to_mgr_tr; create_wallet_and_invest_tr], s) 
    | Some addr -> 
        let transfer_to_wallet : FA12Token.transfer = {
          from = Tezos.get_sender();
          to = (addr: address);
          value = p.amount; 
        } in
        let transfer_to_wallet_tx : operation = Tezos.transaction transfer_to_wallet 0tez invst_tkn_transfer_ep in
        (* get inv ep from smart wallet *)
        let invest_param: SmartWallet.invest_interest_bearing_fa12 = {
          yupana_token_id = (yupana_token_id : nat);
          invst_token_address = (invst_token_details.token_address: address);
          amount = p.amount; 
        } in
        let invest_ep = SmartWallet.get_invest_ep addr in
        let invest_tr = Tezos.transaction invest_param 0tez invest_ep in
        ([transfer_to_wallet_tx; invest_tr],s)
  in return

let main (param, storage : (parameter * Storage.t)) : return = 
  match param with
  | Illuminate p ->illuminate (p, storage)
  | IlluminateWithInterest p -> illuminate_with_interest (p, storage)
  | WithdrawFa12 p -> withdraw (p, storage)
  | AddWalletManager p -> add_wallet_manager (p, storage)
