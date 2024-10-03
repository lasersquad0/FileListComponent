unit Hash;

interface

uses DynamicArray;

resourcestring
  SKeyNotFound = 'Element is not found in Hash!';
  SCannotModifyReadOnly  = 'Cannot modify Read-only hash!';

(***********************************************************)
(*  Hash                                                  *)
(***********************************************************)
 type
  THash<K; V> = class
	private type PointerV = THArrayG<V>.PointerT; // just new name of existing type for better understanding
  protected
   FReadOnly: Boolean;
   FAIndexes: THArrayG<K>;
   FAValues : THArrayG<V>;
   function GetKey(Index: Cardinal): K;
   function GetCount: Cardinal;
  public
   constructor Create; virtual;
//   constructor CreateFromHArrays(IndexHArray:THArraySorted<K>; ValueHArray:THArrayG<V>);
   destructor Destroy; override;
   procedure Clear; virtual;
   procedure ClearMem; virtual;
   procedure SetValue(Key: K; Value: V);
   function GetValue(Key: K): V;
   function GetValuePointer(Key: K): PointerV;
   function IfExist(const Key: K): Boolean;  // check if values with key Key is exists in hash
   function IndexOf(const Key: K): Integer;
   procedure Delete(const Key: K); virtual; // deletes value with key=Key
   property Count: Cardinal read GetCount;
   property Keys[Index: Cardinal]: K read GetKey;
   property Values[Key: K]: V read GetValue write SetValue; default;
   property AIndexes: THArrayG<K> read FAIndexes;
   property AValues: THArrayG<V> read FAValues;
  end;

implementation

uses SysUtils;

constructor THash<K,V>.Create;
begin
  inherited Create;
  FReadOnly := False;
  FAIndexes := THArrayG<K>.Create;
  FAValues  := THArrayG<V>.Create;
end;

{
  constructor THash<K,V>.CreateFromHArrays(IndexHArray:THArraySorted<K>; ValueHArray:THArrayG<V>);
  begin
    inherited Create;
    FAIndexes := IndexHArray;
    FAValues  := ValueHArray;
    FReadOnly := True;
  end;
}

procedure THash<K,V>.Delete(const Key: K);
var n: Integer;
begin
  n := FAIndexes.IndexOf(Key);
  if n >= 0 then begin
    FAIndexes.DeleteValue(n);
    FAValues.DeleteValue(n);
  end;
end;

destructor THash<K,V>.Destroy;
begin
  if not FReadOnly then begin
 	  FAIndexes.Free;
    FAValues.Free;
  end;

  inherited Destroy;
end;

procedure THash<K;V>.Clear;
begin
  FAIndexes.Clear;
  FAValues.Clear;
end;

procedure THash<K,V>.ClearMem;
begin
  FAIndexes.ClearMem;
  FAValues.ClearMem;
end;

function THash<K,V>.GetCount:Cardinal;
begin
  Result := FAIndexes.Count;
end;

function THash<K,V>.GetKey(Index: Cardinal): K;
begin
  Result := FAIndexes[Index];
end;

function THash<K,V>.GetValue(Key: K): V;
var n: Integer;
begin
  n := FAIndexes.IndexOf(Key);
  if n >= 0
    then Result := FAValues[n]
    else raise ERangeError.Create(SKeyNotFound);
end;

function THash<K,V>.GetValuePointer(Key: K): PointerV;
var n: Integer;
begin
  n := FAIndexes.IndexOf(Key);
  if n >= 0
    then Result := FAValues.GetValuePointer(n)
    else raise ERangeError.Create(SKeyNotFound);
end;

function THash<K,V>.IfExist(const Key: K): Boolean;
begin
  Result := FAIndexes.IndexOf(Key) <> -1;
end;

function THash<K,V>.IndexOf(const Key: K): Integer;
begin
  Result := FAIndexes.IndexOf(Key);
end;

procedure THash<K,V>.SetValue(Key: K; Value: V);
var
	n: Integer;
  ind: Cardinal;
begin
  if FReadOnly then raise ERangeError.Create(SCannotModifyReadOnly);

  n := FAIndexes.IndexOf(Key);
  if n >= 0 then begin
 	  FAValues[n] := Value;
  end else begin
	  ind := FAIndexes.AddValue(Key);
	  FAValues.InsertValue(ind, Value);
  end;
end;


end.