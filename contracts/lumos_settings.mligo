(* Settings for time locks  *)

(* make protocol settings more granular? 
  This could help with updating the setitngs as 
  time goes. Better Upgradability, worse performance
 *)

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

module ProtocolSettings = struct 
  type settings_version = V1 | V2 | V3 | V4 (* subject to change, figure out how many? or just make an arbitrary string *)

  type min_max_deposits = nat * nat
  
  type settings = {
    version : settings_version;
    time_lock :  timestamp;
    min_max_deposits : min_max_deposits;
    annuity : nat;
    annuity_on_interest : nat;
  }
  
  type t = {
    settings : (InvestmentToken.token_id, settings) big_map;
  }
end


module NftToken = struct 
  type token_id = nat 
  type token_address = address
end

module Storage = struct 
  type t = {
    owner_settings_map : (NftToken.token_address * NftToken.token_id, ProtocolSettings.t) big_map; 
    allowed_tokens : (NftToken.token_address * NftToken.token_id, string list) big_map ;
  }
end

type get_token_settings_param = {
  nft_address : address;
  nft_id : nat;
  invst_token_id : InvestmentToken.token_id;
}

type set_token_settings_param = {
  nft_address : address;
  nft_id : nat;
  invst_token_id : InvestmentToken.token_id;
}

type return = (operation list * Storage.t)

(* View the settings of a specific Investment Token inside an NFT 
[@view]
let get_token_settings (p,s : get_token_settings_param * Storage.t) : ProtocolSettings.settings option = 
  let token_details_map = match Big_map.find_opt ((p.nft_address, p.nft_id)) s.owner_settings_map with
  | Some t_dts_map -> t_dts_map
  | None -> (failwith ("TOKEN_DETAILS_MAP_NOT_FOUND")) in
  Big_map.find_opt (p.invst_token_id) token_details_map
*)

(* List of tokens that are allowed in the NFT 
[@view] (*todo  change based on the type you decide *)
let get_allowed_tokens (p, s : get_allowed_tokens_param * Storage.t) : string list option = 
  Big_map.find_opt ((p.nft_address, p.nft_id)) s.allowed_tokens


(* Set the settings of a specific Investment Token inside an NFT *)
let set_settings (p,s : set_token_settings_param * Storage.t ) : return = 
  (* check if settings conditions are met*)
  let new_owner_settings_map =
    Big_map.update (p.nft_address, p.nft_id) (Some p.settings) s.new_owner_settings_map in
  ([], { s with owner_settings_map = new_owner_settings_map })
