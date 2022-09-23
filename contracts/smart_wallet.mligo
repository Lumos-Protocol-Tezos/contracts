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

module NftToken = struct 
  type token_id = nat 
  type token_address = address
end

module FA12Token = struct 
  type transfer = [@layout:comb] {
    from: address;
    to : address;
    value: nat
  }
  type allow = {
    spender : address;
    value : nat;
  }
  let get_transfer_ep (addr : InvestmentToken.token_address) : transfer contract = 
    match (Tezos.get_entrypoint_opt ("%transfer") (addr : address) : transfer contract option) with
    | None -> (failwith "TRANSFER_EP_NOT_FOUND")
    | Some ep -> ep

  let get_allow_ep (addr: InvestmentToken.token_address) : allow contract = 
    match (Tezos.get_entrypoint_opt ("%approve") addr : allow contract option) with 
      Some ep -> ep
    | None -> (failwith "ALLOW_EP_NOT_FOUND" : allow contract)
end

module WalletManager = struct 
  type wm_address = address
end

module YupanaPfRouter = struct 
  let get_get_price_ep (addr: address): nat set contract = 
    match (Tezos.get_entrypoint_opt ("%getPrice") (addr): nat set contract option) with 
      Some ep -> ep
    | None -> (failwith "GET_PRICE_EP_NOT_FOUND" : nat set contract)
end

module Yupana = struct 
  type mint = [@layout:comb]{
    token_id: nat ; 
    amount: nat ;
    min_received: nat ;
  }
  type redeem = [@layout:comb]{
    token_id: nat ; 
    amount: nat ;
    min_received: nat ;
  }

  let get_update_interest_ep (addr: address): nat contract = 
    match (Tezos.get_entrypoint_opt ("%updateInterest") (addr): nat contract option) with 
      None -> (failwith "UPDATE_INTEREST_EP_NOT_FOUND")
    | Some ep -> ep

  let get_mint_ep (addr: address): mint contract =
    match (Tezos.get_entrypoint_opt ("%mint") (addr) : mint contract option) with 
    | None -> (failwith "MINT_EP_NOT_FOUND")
    | Some contract -> contract 

  let get_redeem_ep (addr : address) : redeem contract = 
    match (Tezos.get_entrypoint_opt ("%redeem") (addr) : redeem contract option) with 
    | None -> (failwith "REDEEM_EP_NOT_FOUND")
    | Some contract -> contract 

  let get_approve_tr (invst_token_address, amount : address * nat) : operation = 
    let yupana_address: address = ("KT1PW3aKxfB89HUrq8ywnw9tLvxtuHLgsjJW": address) in
    let allow_ep: FA12Token.allow contract = FA12Token.get_allow_ep invst_token_address in
    let allow_p : FA12Token.allow = {
      spender = yupana_address;
      value = amount
    } in
    Tezos.transaction allow_p 0tez allow_ep 

  let get_price_tr (token_id_set: nat set) : operation = 
    let pf_router_address = ("KT1MZeSimmt1A3omsJXjKy9ihma1ajUPqD4m" : address) in 
    let get_price_ep: nat set contract = YupanaPfRouter.get_get_price_ep pf_router_address in
    Tezos.transaction token_id_set 0tez get_price_ep 
  
  let get_update_interest_tr (token_id : nat) : operation = 
    let yupana_address: address = ("KT1PW3aKxfB89HUrq8ywnw9tLvxtuHLgsjJW": address) in
    let update_interest_ep : nat contract = get_update_interest_ep yupana_address in
    Tezos.transaction token_id 0tez update_interest_ep

  let get_mint_tr (p: mint) :operation = 
    let yupana_address: address = ("KT1PW3aKxfB89HUrq8ywnw9tLvxtuHLgsjJW": address) in
    let mint_ep: mint contract = get_mint_ep yupana_address in 
    Tezos.transaction p 0tez mint_ep

  let get_redeem_tr (p: redeem): operation = 
    let yupana_address: address = ("KT1PW3aKxfB89HUrq8ywnw9tLvxtuHLgsjJW": address) in
    let redeem_ep: redeem contract = get_redeem_ep yupana_address in
    Tezos.transaction p 0tez redeem_ep

end

module Storage = struct 
  type t = {
    wallet_manager : WalletManager.wm_address;
    nft_address : NftToken.token_address * NftToken.token_id;
  }
end
type return = operation list * Storage.t

type withdraw_fa12_param = {
  receiver_address : address;
  invst_token_address : InvestmentToken.token_address;
  amount : nat;
}

type invest_interest_bearing_fa12_param = {
  yupana_token_id: nat;
  invst_token_address : address;
  amount : nat;
}

type withdraw_interest_bearing_fa12_param = {
  yupana_token_id: nat;
  invst_token_address : address;
  amount : nat;
  receiver_address : address;
}
type parameter = 
  WithdrawFa12 of withdraw_fa12_param 
  | InvestInterestBearingFa12 of invest_interest_bearing_fa12_param
  | InvestInterestBearingFa12S2 of invest_interest_bearing_fa12_param
  | WithdrawInterestBearingFa12 of withdraw_interest_bearing_fa12_param 
  | WithdrawInterestBearingFa12S2 of withdraw_interest_bearing_fa12_param 
  | WithdrawInterestBearingFa12S3 of withdraw_interest_bearing_fa12_param 

let withdraw_fa12 (p,s : withdraw_fa12_param * Storage.t) : return = 
  if Tezos.get_sender() <> s.wallet_manager then (failwith "ONLY_WALLET_MANAGER_ALLOWED")
  else 
    let invst_tkn_transfer_ep : FA12Token.transfer contract = FA12Token.get_transfer_ep p.invst_token_address in 
    let transfer_param : FA12Token.transfer = {
      from = Tezos.get_self_address();
      to = p.receiver_address;
      value = p.amount; 
    } in
    let transfer_txn : operation = Tezos.transaction transfer_param 0tez invst_tkn_transfer_ep in
    let new_wallet_map = s in (* TODO *)
    ([transfer_txn], new_wallet_map)


let invest_interest_bearing_fa12 (p,s : invest_interest_bearing_fa12_param * Storage.t) : return = 
  (* allow *)
  let allow_tr = Yupana.get_approve_tr (p.invst_token_address, p.amount) in
  let get_price_tr = Yupana.get_price_tr (Set.literal [p.yupana_token_id;]) in
  let update_interest_tr = Yupana.get_update_interest_tr p.yupana_token_id in
  let continue_execution_tr = 
    Tezos.transaction p 0tez (Tezos.self "%investInterestBearingFa12S2": invest_interest_bearing_fa12_param contract) in
  ([allow_tr; get_price_tr; update_interest_tr; continue_execution_tr;], s)

let invest_interest_bearing_fa12_step2 (p,s : invest_interest_bearing_fa12_param * Storage.t) : return =
  let mint_param = {
    token_id = p.yupana_token_id;
    amount = p.amount;
    min_received = 1n;
  } in
  let mint_tr = Yupana.get_mint_tr mint_param in
  ([mint_tr], s)

let withdraw_interest_bearing_fa12 (p,s : (withdraw_interest_bearing_fa12_param * Storage.t)) : return =
  let get_price_tr = Yupana.get_price_tr (Set.literal [0n; p.yupana_token_id;]) in
  let update_interest_tr = Yupana.get_update_interest_tr p.yupana_token_id in
  let update_interest_tr2 = Yupana.get_update_interest_tr 0n in
  let continue_execution_tr = 
    Tezos.transaction p 0tez (Tezos.self "%withdrawInterestBearingFa12S2": withdraw_interest_bearing_fa12_param contract) in
  ([get_price_tr; update_interest_tr; update_interest_tr2; continue_execution_tr;], s)

let withdraw_interest_bearing_fa12_step2 (p,s : (withdraw_interest_bearing_fa12_param * Storage.t)) : return = 
  let redeem_param = {
    token_id = p.yupana_token_id;
    amount = p.amount;
    min_received = 1n;
  } in
  let redeem_tr = Yupana.get_redeem_tr redeem_param in
  let continue_execution_tr = 
    Tezos.transaction p 0tez (Tezos.self "%withdrawInterestBearingFa12S3": withdraw_interest_bearing_fa12_param contract) in
  ([redeem_tr; continue_execution_tr], s)


let withdraw_interest_bearing_fa12_step3 (p,s : (withdraw_interest_bearing_fa12_param * Storage.t)) : return =
  let invst_tkn_transfer_ep : FA12Token.transfer contract = FA12Token.get_transfer_ep p.invst_token_address in 
  let transfer_param : FA12Token.transfer = {
    from = Tezos.get_self_address();
    to = p.receiver_address;
    value = p.amount; 
  } in
  let transfer_txn : operation = Tezos.transaction transfer_param 0tez invst_tkn_transfer_ep in
  ([transfer_txn], s)

let main (param, storage : parameter * Storage.t) : return = match param with
  | WithdrawFa12 p -> withdraw_fa12 (p, storage)
  | InvestInterestBearingFa12 p -> invest_interest_bearing_fa12 (p, storage)
  | InvestInterestBearingFa12S2 p -> invest_interest_bearing_fa12_step2 (p,storage)
  | WithdrawInterestBearingFa12 p -> withdraw_interest_bearing_fa12 (p, storage)
  | WithdrawInterestBearingFa12S2 p -> withdraw_interest_bearing_fa12_step2 (p, storage)
  | WithdrawInterestBearingFa12S3 p -> withdraw_interest_bearing_fa12_step3 (p, storage)
