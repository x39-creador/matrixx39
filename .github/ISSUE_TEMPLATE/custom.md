


import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Int "mo:base/Int";
import Float "mo:base/Float";
import List "mo:base/List";
import Option "mo:base/Option";
import Result "mo:base/Result";
import Error "mo:base/Error";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import Buffer "mo:base/Buffer";
import Array "mo:base/Array";
import Time "mo:base/Time";
import Cycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import TrieMap "mo:base/TrieMap";
import Hex "mo:base/Hex";
import Blob "mo:base/Blob";
import Text "mo:base/Text";

// ICRC-1 + ICRC-2 INTERFACES
public module ICRC1 {
  public type Account = { owner: Principal; subaccount: ?Blob };
  public type BalanceRequest = { account: Account };
  public type Balance = Nat;
  public type TransferRequest = {
    from_subaccount: ?Blob;
    to: Account;
    fee: ?Nat;
    memo: ?Blob;
    amount: Nat;
    created_at_time: ?Nat64;
  };
  public type TransferResponse = { #Ok: Nat64; #Err: TransferError };
  public type TransferError = {
    #TooOld;
    #CreatedInFuture;
    #Duplicate;
    #TemporarilyUnavailable;
    #GenericError: Text;
    #InsufficientFunds: { expected: Nat; balance: Nat };
  };
};

public module ICRC2 {
  public type AllowanceRequest = { account: ICRC1.Account; spender: Principal };
  public type Allowance = { allowance: Nat; expires_at: ?Nat64 };
  public type ApproveRequest = {
    from_subaccount: ?Blob;
    spender: Principal;
    expires_at: ?Nat64;
    expected_allowance: ?Nat;
    fee: ?Nat;
    memo: ?Blob;
    amount: Nat;
    created_at_time: ?Nat64;
  };
  public type TransferFromRequest = {
    from_subaccount: ?Blob;
    from: ICRC1.Account;
    to: ICRC1.Account;
    fee: ?Nat;
    memo: ?Blob;
    amount: Nat;
    created_at_time: ?Nat64;
    spender: Principal;
  };
};

// TU WALLET Y CANISTER
let walletHex: Text = "d4c065a355744a60eec587031b633448deb641c80c4299f9782fd41572d42ba2";
let walletBlob: Blob = Blob.fromArray(Hex.decode_unsafe(walletHex));
let miWallet: ICRC1.Account = { owner = Principal.fromBlob(walletBlob); subaccount = null };
let targetCanister: Principal = Principal.fromText("7m3l2-sqaaa-aaaan-q24pa-cai");

// ESTADO TOKEN ICRC-2 X-39 MATRIX
stable var totalSupply: Nat = 0;
stable var decimals: Nat8 = 8;
let balances = TrieMap.TrieMap<ICRC1.Account, Nat>(func(a:ICRC1.Account, b:ICRC1.Account): Bool { a == b }, func(a:ICRC1.Account): Hash.Hash { Hash.hash(a.owner) });
let allowances = TrieMap.TrieMap<(ICRC1.Account, Principal), ICRC2.Allowance>(func(t:(ICRC1.Account, Principal), u:(ICRC1.Account, Principal)): Bool { t.0 == u.0 and t.1 == u.1 }, func(t:(ICRC1.Account, Principal)): Hash.Hash { Hash.hashNat64(Hash.hash(t.1)) });

// LEDGER ICP
public type ICPAccount = { owner: Principal; subaccount: ?Blob };
public type ICPTokens = { e8s: Nat64 };
public type ICPTransferArg = { memo: ?Nat64; fee: ?ICPTokens; from_subaccount: ?Blob; to: ICPAccount; amount: ICPTokens; created_at_time: ?Nat64 };
public type ICPTransferResult = { #Ok: Nat64; #Err: Text };
let ICP_LEDGER: Principal = Principal.fromText("ryjl3-tyaaa-aaaaa-aaaba-cai");
stable var ledgerCanister: ?actor { account_balance: (ICPAccount) -> async ICPTokens; transfer: (ICPTransferArg) -> async ICPTransferResult } = null;

// ICRC-1 METHODS
public shared query func icrc1_balance_of(req: ICRC1.BalanceRequest): async Nat {
  switch (balances.get(req.account)) {
    case (?b) b;
    case null 0;
  }
};

public shared(msg) func icrc1_transfer(args: ICRC1.TransferRequest): async ICRC1.TransferResponse {
  let caller = Principal.fromActor(matrix);
  let fromAccount: ICRC1.Account = switch (args.from_subaccount) {
    case (?sub) { owner = caller; subaccount = ?sub };
    case null { owner = caller; subaccount = null };
  };
  let fromBalance = switch (balances.get(fromAccount)) {
    case (?b) b;
    case null 0;
  };
  if (fromBalance < args.amount) {
    return #Err(#InsufficientFunds({ expected = args.amount; balance = fromBalance }));
  };
  let fee = switch (args.fee) { case (?f) f; case null 0 };
  let toBalance = switch (balances.get(args.to)) {
    case (?b) b;
    case null 0;
  };
  balances.put(fromAccount, fromBalance - args.amount - fee);
  balances.put(args.to, toBalance + args.amount);
  #Ok(Nat64.fromNatWrap(Time.now()))
};

// ICRC-2 APPROVE
public shared(msg) func icrc2_approve(args: ICRC2.ApproveRequest):
