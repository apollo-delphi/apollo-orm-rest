unit Apollo_ORM_REST;

interface

uses
  Apollo_DB_Core,
  Apollo_DB_Utils,
  Apollo_HTTP,
  Apollo_ORM,
  IdCustomHTTPServer,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  System.Rtti;

type
  [SkipStructure]
  TEntityREST = class(TEntityFeatID)
  private
    FRESTClient: TRESTClient;
    function GetCheckIsValueUniqueRoute(const aPropName: string; const aValue: Variant): string;
    function GetCurrentInstance: TInstance;
    procedure UpdateProps(aInstance: TInstance);
  protected
    procedure DeleteToDB; override;
    procedure InsertToDB; override;
    procedure UpdateToDB; override;
  public
    function CheckIsValueUnique(const aPropName: string; const aValue: Variant): Boolean; override;
    constructor Create(aRESTClient: TRESTClient; const aID: Integer); overload;
  end;

  TEntityRESTClass = class of TEntityREST;

  TEntityListAbstractREST<T: TEntityREST> = class(TEntityListBase<T>)
  protected
    procedure DoCreateOnServerSide(aDBEngine: TDBEngine; aBuilder: IEntityListBuilder); overload;
    procedure DoCreateOnServerSide(aOwnerEntity: TEntityREST; const aOrderBy: POrder); overload;
  public
    constructor Create(const aOwnsObjects: Boolean = True); reintroduce;
  end;

{$M+}
  TLoadParams = class
  private
    FLimit: Integer;
    FOffset: Integer;
    FOrder: string;
    FOrderDirection: TOrderDirection;
    function PropExists(const aPropName: string): Boolean;
    function PropValueAsStr(const aPropName: string): string;
  public
    function GetOrderItem: TOrderItem;
  published
    property Limit: Integer read FLimit write FLimit;
    property Offset: Integer read FOffset write FOffset;
    property Order: string read FOrder write FOrder;
    property OrderDirection: TOrderDirection read FOrderDirection write FOrderDirection;
  end;
{$M-}

  TLoadParamsClass = class of TLoadParams;

  TRefListLoadParams = class(TLoadParams)
  private
    FListClass: TClass;
    FPropName: string;
    FPropValue: Integer;
  published
    property ListClass: TClass read FListClass write FListClass;
    property PropName: string read FPropName write FPropName;
    property PropValue: Integer read FPropValue write FPropValue;
  end;

  TEntityListREST<T: TEntityREST> = class(TEntityListAbstractREST<T>)
  private
    FIsServerSide: Boolean;
    FLoadParams: TLoadParams;
    FNextRecNum: Integer;
    FRESTClient: TRESTClient;
    procedure LoadOnClientSide(const aOffset: Integer);
  protected
    procedure LoadOnServerSide(const aLoadParams: TLoadParams; var aBuilder: IEntityListBuilder); virtual;
  public
    function GetInstances: TArray<TInstance>;
    function GetNextRecNum: Integer; override;
    procedure LoadMore; override;
    constructor Create(aDBEngine: TDBEngine; aLoadParams: TLoadParams); overload;
    constructor Create(aOwnerEntity: TEntityREST; const aOrderBy: POrder = nil); overload;
    constructor Create(aRESTClient: TRESTClient; aLoadParams: TLoadParams); overload;
    constructor CreateOnServerSide(aDBEngine: TDBEngine; const aLoadParams: TLoadParams);
    destructor Destroy; override;
  end;

  TRESTRouter = class
  private
    FRegisteredClasses: TArray<TClass>;
    class function EncodeID(aEntityREST: TEntityREST): string;
    class function EncodeLoadParams(LoadParams: TLoadParams): string;
    function DoEncodeLoadParams(LoadParams: TLoadParams): string;
    function HandleIsUniqueRequest(aEntityREST: TEntityREST; const aRequestURI: string): string;
    function MatchRequestRoute(const aTemplateRoute, aRequestRoute: string;
      const aParams: TDictionary<string, Variant>): Boolean;
    function NormTemplateRoute(const aTemplateRoute: string): string;
    function DoDecodeLoadParams(const aRequestRoute: string; out aEntityListClass: TClass;
      out LoadParams: TLoadParams): Boolean;
    function TryGetAutoRouteValues(aClass: TClass; out aTemplateRoute: string;
      out aLoadParamsClass: TLoadParamsClass): Boolean;
    function TryGetEntityListRoute(aRequestInfo: TIdHTTPRequestInfo; aDBEngine: TDBEngine;
      out aRespose: string): Boolean;
    function TryGetEntityRoute(aRequestInfo: TIdHTTPRequestInfo; aDBEngine: TDBEngine;
      out aRespose: string): Boolean;
    function TryGetRefRouteValues(aLoadParams: TLoadParams; aRegistredClass: TClass; out aTemplateRoute: string;
      out aLoadParamsClass: TLoadParamsClass): Boolean;
  public
    class function GetUnknownRouteResponse: string;
    class function TryFindAutoRoute(aRequestInfo: TIdHTTPRequestInfo; aDBEngine: TDBEngine;
      out aRespose: string): Boolean;
    class procedure RegisterForRouting(aClass: TClass);
    constructor Create;
  end;

  AutoRoute = class(TCustomAttribute)
  private
    FLoadParamsClass: TLoadParamsClass;
    FTemplateRoute: string;
  public
    constructor Create(const aTemplateRoute: string; aLoadParamsClass: TLoadParamsClass = nil);
  end;

  TRESTMessage = record
  private
    FHasModifiedFields: Boolean;
  public
    CanLoadMore: Boolean;
    Data: TArray<Variant>;
    Instances: TArray<TInstance>;
    NextRecNum: Integer;
    function HasModifiedFields: Boolean;
    procedure Free;
    procedure Init;
    constructor Create(const aEntities: TArray<TEntityREST>);
    constructor CreateForID(const aEntities: TArray<TEntityREST>; dummy: Integer = 0);
    constructor CreateForModifiedFields(const aEntities: TArray<TEntityREST>;
      dummy1: Integer = 0; dummy2: Integer = 0);
  end;

  TJSONSerializer = class
  private
    class function CreateInstance(aRttiType: TRttiType): TValue; static;
    class function DeserializeArray(aRttiArrayType: TRttiDynamicArrayType; aJSON: TJSONArray): TValue; static;
    class function DeserializeObject(aRttiType: TRttiType; var aInstance: TValue; aJSON: TJSONObject): TValue; static;
    class function DeserializeStream(const aJSONValue: string): TStream; static;
    class function DeserializeType(const aTypeName: string; aRttiType: TRttiType; aJSON: TJSONValue): TValue; static;
    class function GetPointer(aInstance: TValue): Pointer; static;
    class function GetValue(aRttiMember: TRttiMember; aInstance: TValue): TValue; static;
    class function SerializeStream(const aTypeName: string; aStream: TStream): TJSONPair; static;
    class procedure SerializeArray(aRttiArrayType: TRttiDynamicArrayType; aArray: TValue; aJSON: TJSONArray); static;
    class procedure SerializeObject(aRttiType: TRttiType; aInstance: TValue; aJSON: TJSONObject); static;
    class procedure SerializeType(const aTypeName: string; aRttiType: TRttiType; aValue: TValue; aJSON: TJSONObject); static;
  public
    class procedure Deserialize<T>(var aObject: T; const aJSONString: string);
    class function Serialize<T>(aObject: T): string;
  end;

var
  gRESTRouter: TRESTRouter;

implementation

uses
  Apollo_Helpers,
  System.NetEncoding,
  System.SysUtils,
  System.TypInfo,
  System.Variants;

{ TEntityListAbstractREST<T> }

constructor TEntityListAbstractREST<T>.Create(const aOwnsObjects: Boolean);
begin
  inherited Create(aOwnsObjects);
end;

procedure TEntityListAbstractREST<T>.DoCreateOnServerSide(aDBEngine: TDBEngine;
  aBuilder: IEntityListBuilder);
begin
  inherited Create(aDBEngine, aBuilder);
end;

procedure TEntityListAbstractREST<T>.DoCreateOnServerSide(
  aOwnerEntity: TEntityREST; const aOrderBy: POrder);
begin
  inherited Create(aOwnerEntity, aOrderBy);
end;

{ TEntityListREST<T> }

constructor TEntityListREST<T>.Create(aRESTClient: TRESTClient;
  aLoadParams: TLoadParams);
begin
  inherited Create(True);
  FRESTClient := aRESTClient;
  FLoadParams := aLoadParams;

  LoadOnClientSide(aLoadParams.Offset);

  AfterCreate;
end;

constructor TEntityListREST<T>.Create(aOwnerEntity: TEntityREST;
  const aOrderBy: POrder);
var
  LoadParams: TRefListLoadParams;
  FKey: TFKey;
  FKeys: TFKeys;
begin
  if Assigned(aOwnerEntity.FRESTClient) then
  begin
    LoadParams := TRefListLoadParams.Create;
    if Assigned(aOrderBy) then
    begin
      LoadParams.Order := aOrderBy^.OrderItems[0].FieldName;
      LoadParams.OrderDirection := aOrderBy^.OrderItems[0].Direction;
    end;

    if DoCreateRefList(aOwnerEntity, aOrderBy,
      procedure(const aFieldName: string; const aFKey: TFKey)
      begin
        LoadParams.PropName := aFKey.PropName;
        LoadParams.PropValue := aOwnerEntity.Prop[aFKey.ReferPropName];
        LoadParams.ListClass := ClassType;
      end
    )
    then
      Create(aOwnerEntity.FRESTClient, LoadParams);
  end
  else
    DoCreateOnServerSide(aOwnerEntity, aOrderBy);
end;

constructor TEntityListREST<T>.CreateOnServerSide(aDBEngine: TDBEngine;
  const aLoadParams: TLoadParams);
begin
  Create(aDBEngine, aLoadParams);
end;

destructor TEntityListREST<T>.Destroy;
begin
  if Assigned(FLoadParams) then
    FLoadParams.Free;

  inherited;
end;

function TEntityListREST<T>.GetInstances: TArray<TInstance>;
var
  Entity: TEntityREST;
begin
  Result := [];

  for Entity in Self do
    Result := Result + [Entity.GetCurrentInstance];
end;

function TEntityListREST<T>.GetNextRecNum: Integer;
begin
  if FIsServerSide then
    Result := inherited GetNextRecNum
  else
    Result := FNextRecNum;
end;

procedure TEntityListREST<T>.LoadMore;
begin
  if FIsServerSide then
    inherited LoadMore
  else
    LoadOnClientSide(GetNextRecNum);
end;

procedure TEntityListREST<T>.LoadOnClientSide(const aOffset: Integer);
var
  Entity: TEntityREST;
  Instance: TInstance;
  Instances: TArray<TInstance>;
  RESTMessage: TRESTMessage;
  sJSON: string;
begin
  FLoadParams.Offset := aOffset;
  sJSON := FRESTClient.Get(TRESTRouter.EncodeLoadParams(FLoadParams));

  RESTMessage.Init;
  TJSONSerializer.Deserialize<TRESTMessage>(RESTMessage, sJSON);
  FCanLoadMore := RESTMessage.CanLoadMore;
  FNextRecNum := RESTMessage.NextRecNum;

  for Instance in RESTMessage.Instances do
  begin
    Entity := TEntityRESTClass(GetEntityClass).CreateByInstance(nil{FDBEngine}, Instance);
    Entity.FRESTClient := FRESTClient;
    Add(Entity);
  end;
end;

procedure TEntityListREST<T>.LoadOnServerSide(const aLoadParams: TLoadParams;
  var aBuilder: IEntityListBuilder);
begin
end;

constructor TEntityListREST<T>.Create(aDBEngine: TDBEngine;
  aLoadParams: TLoadParams);
var
  Builder: IEntityListBuilder;
begin
  FIsServerSide := True;

  Builder := MakeEntityListBuilder('T')
    .SetLimit(aLoadParams.Limit)
    .SetOffset(aLoadParams.FOffset);

  try
    if aLoadParams is TRefListLoadParams then
    begin
      Builder
        .AddAndWhere('T', TRefListLoadParams(aLoadParams).PropName, eEquals, 'KeyVal')
        .SetParam('KeyVal', TRefListLoadParams(aLoadParams).PropValue);
    end
    else
      LoadOnServerSide(aLoadParams, Builder);
  finally
    aLoadParams.Free;
  end;

  DoCreateOnServerSide(aDBEngine, Builder);
end;

{ TRESTRouter }

constructor TRESTRouter.Create;
begin
  FRegisteredClasses := [];
end;

function TRESTRouter.DoEncodeLoadParams(LoadParams: TLoadParams): string;
var
  i: Integer;
  Key: string;
  LoadParamsClass: TLoadParamsClass;
  RegisteredClass: TClass;
  TemplateRoute: string;
  TemplateRouteWords: TArray<string>;
begin
  for RegisteredClass in FRegisteredClasses do
  begin
    if RegisteredClass.InheritsFrom(TEntityAbstract) then
      Continue;

    if TryGetRefRouteValues(LoadParams, RegisteredClass, {out}TemplateRoute, {out}LoadParamsClass) or
       TryGetAutoRouteValues(RegisteredClass, {out}TemplateRoute, {out}LoadParamsClass)
    then
    begin
      if LoadParams is LoadParamsClass then
      begin
        TemplateRoute := NormTemplateRoute(TemplateRoute);
        TemplateRouteWords := TemplateRoute.Split(['/']);

        for i := 0 to TemplateRouteWords.Count - 1 do
        begin
          Key := TStringTools.SubStrByKey(TemplateRouteWords[i], '{', '}');
          if not Key.IsEmpty and LoadParams.PropExists(Key) then
            TemplateRouteWords[i] := LoadParams.PropValueAsStr(Key);
        end;

        Result := string.Join('/', TemplateRouteWords);
        Exit(Result);
      end;
    end;
  end;

  raise Exception.CreateFmt('TRESTRouter.DoEncodeLoadParams: did not find EntityList linked with %s', [LoadParams.ClassName]);
end;

class function TRESTRouter.EncodeID(aEntityREST: TEntityREST): string;
var
  aLoadParamsClass: TLoadParamsClass;
  i: Integer;
  TemplateRouteWords: TArray<string>;
begin
  if gRESTRouter.TryGetAutoRouteValues(aEntityREST.ClassType, {out}Result, {out}aLoadParamsClass) then
  begin
    Result := gRESTRouter.NormTemplateRoute(Result);
    TemplateRouteWords := Result.Split(['/']);

    for i := 0 to TemplateRouteWords.Count - 1 do
    begin
      if TemplateRouteWords[i].ToLower = '{id}' then
        TemplateRouteWords[i] := aEntityREST.ID.ToString;
    end;

    Result := string.Join('/', TemplateRouteWords);
  end
  else
    raise Exception.CreateFmt('TRESTRouter.EncodeID: did not find AutoRoute attribute in class %s', [ClassName]);
end;

class function TRESTRouter.EncodeLoadParams(LoadParams: TLoadParams): string;
begin
  Result := gRESTRouter.DoEncodeLoadParams(LoadParams);
end;

class function TRESTRouter.GetUnknownRouteResponse: string;
begin
  Result := '404 Unknown route.';
end;

function TRESTRouter.HandleIsUniqueRequest(aEntityREST: TEntityREST;
  const aRequestURI: string): string;
var
  IsUnique: Boolean;
  Params: string;
  RESTMessage: TRESTMessage;
  Words: TArray<string>;
begin
  Params := TStringTools.SubStrByKey(aRequestURI, '/isunique/', '');
  Words := Params.Split(['/']);

  if Words.Count <> 2 then
    raise Exception.Create('TRESTRouter.HandleIsUniqueRequest: route for isunique must have 2 params.');

  IsUnique := aEntityREST.CheckIsValueUnique(Words[0], Words[1]);

  RESTMessage.Init;
  RESTMessage.Data := [IsUnique];
  Result := TJSONSerializer.Serialize<TRESTMessage>(RESTMessage);
end;

function TRESTRouter.MatchRequestRoute(const aTemplateRoute,
  aRequestRoute: string; const aParams: TDictionary<string, Variant>): Boolean;
var
  i: Integer;
  Key: string;
  RequestRouteWords: TArray<string>;
  TemplateRoute: string;
  TemplateRouteWords: TArray<string>;
  Value: string;
begin
  Result := False;
  TemplateRoute := NormTemplateRoute(aTemplateRoute);

  RequestRouteWords := aRequestRoute.Split(['/']);
  TemplateRouteWords := TemplateRoute.Split(['/']);

  for i := 0 to TemplateRouteWords.Count - 1 do
    if TemplateRouteWords[i].ToUpper.Contains('{') then
    begin
      if RequestRouteWords.Count <= i then
        Break;

      Key := TStringTools.SubStrByKey(TemplateRouteWords[i], '{', '}');
      Value := RequestRouteWords[i];

      if StrToIntDef(Value, 0) > 0 then
        aParams.Add(Key, Value.ToInteger)
      else
      if Value <> '' then
        aParams.Add(Key, Value);
    end
    else
    if RequestRouteWords[i].ToUpper <> TemplateRouteWords[i].ToUpper then
      Exit;

  Result := True;
end;

function TRESTRouter.NormTemplateRoute(const aTemplateRoute: string): string;
begin
  Result := aTemplateRoute;

  if not Result.StartsWith('/') then
    Result := '/' + Result;
end;

class procedure TRESTRouter.RegisterForRouting(aClass: TClass);
begin
  gRESTRouter.FRegisteredClasses := gRESTRouter.FRegisteredClasses + [aClass];
end;

function TRESTRouter.DoDecodeLoadParams(const aRequestRoute: string; out aEntityListClass: TClass;
  out LoadParams: TLoadParams): Boolean;
var
  IsRefList: Boolean;
  LoadParamsClass: TLoadParamsClass;
  Params: TDictionary<string, Variant>;
  ParamsPair: TPair<string, Variant>;
  RegisteredClass: TClass;
  TemplateRoute: string;
begin
  Result := False;

  for RegisteredClass in FRegisteredClasses do
  begin
    if RegisteredClass.InheritsFrom(TEntityAbstract) then
      Continue;

    IsRefList := aRequestRoute.StartsWith('/reference/');

    if IsRefList and TryGetRefRouteValues(nil{LoadParams}, RegisteredClass, {out}TemplateRoute, {out}LoadParamsClass) or
       TryGetAutoRouteValues(RegisteredClass, {out}TemplateRoute, {out}LoadParamsClass)
    then
    begin
      Params := TDictionary<string, Variant>.Create;
      try
        if MatchRequestRoute(TemplateRoute, aRequestRoute, Params) then
        begin
          aEntityListClass := RegisteredClass;

          LoadParams := LoadParamsClass.Create;
          try
            for ParamsPair in Params do
              if LoadParams.PropExists(ParamsPair.Key) then
                SetPropValue(LoadParams, ParamsPair.Key, ParamsPair.Value);

            Exit(True);
          except
            LoadParams.Free;
            raise;
          end;
        end;
      finally
        Params.Free;
      end;
    end;
  end;
end;

class function TRESTRouter.TryFindAutoRoute(aRequestInfo: TIdHTTPRequestInfo;
  aDBEngine: TDBEngine; out aRespose: string): Boolean;
begin
  Result := False;

  try
    if gRESTRouter.TryGetEntityRoute(aRequestInfo, aDBEngine, {out}aRespose) then
      Result := True
    else
    if gRESTRouter.TryGetEntityListRoute(aRequestInfo, aDBEngine, {out}aRespose) then
      Result := True;
  except
    on E: Exception do
    begin
      Result := True;
      aRespose := 'Error: ' + E.Message;
    end;
  end;
end;

function TRESTRouter.TryGetAutoRouteValues(aClass: TClass;
  out aTemplateRoute: string; out aLoadParamsClass: TLoadParamsClass): Boolean;
var
  Attribute: TCustomAttribute;
  RttiContext: TRttiContext;
begin
  Result := False;

  RttiContext := TRttiContext.Create;
  try
    for Attribute in RttiContext.GetType(aClass.ClassInfo).GetAttributes do
      if Attribute is AutoRoute then
      begin
        aTemplateRoute := AutoRoute(Attribute).FTemplateRoute;
        aLoadParamsClass := AutoRoute(Attribute).FLoadParamsClass;
        Exit(True);
      end;
  finally
    RttiContext.Free;
  end;
end;

function TRESTRouter.TryGetEntityListRoute(aRequestInfo: TIdHTTPRequestInfo;
  aDBEngine: TDBEngine; out aRespose: string): Boolean;
var
  CanLoadMoreMethod: TRttiMethod;
  ConstructorMethod: TRttiMethod;
  EntityList: TObject;
  EntityListClass: TClass;
  GetInstancesMethod: TRttiMethod;
  GetNextRecNumMethod: TRttiMethod;
  LoadParams: TLoadParams;
  OutValue: Integer;
  RESTMessage: TRESTMessage;
  RttiContext: TRttiContext;
begin
  Result := False;
  RESTMessage.Init;
  OutValue := 0;

  RttiContext := TRttiContext.Create;
  try
    if DoDecodeLoadParams(aRequestInfo.URI, {out}EntityListClass, {out}LoadParams) then
    begin
      ConstructorMethod := RttiContext.GetType(EntityListClass).GetMethod('CreateOnServerSide');
      EntityList := ConstructorMethod.Invoke(EntityListClass, [aDBEngine, LoadParams]).AsObject;
      try
        GetInstancesMethod := RttiContext.GetType(EntityListClass).GetMethod('GetInstances');
        RESTMessage.Instances := GetInstancesMethod.Invoke(EntityList, []).AsType<TArray<TInstance>>;

        CanLoadMoreMethod := RttiContext.GetType(EntityListClass).GetMethod('CanLoadMore');
        RESTMessage.CanLoadMore := CanLoadMoreMethod.Invoke(EntityList, [OutValue]).AsType<Boolean>;

        GetNextRecNumMethod := RttiContext.GetType(EntityListClass).GetMethod('GetNextRecNum');
        RESTMessage.NextRecNum := GetNextRecNumMethod.Invoke(EntityList, []).AsType<Integer>;

        aRespose := TJSONSerializer.Serialize<TRESTMessage>(RESTMessage);

        Exit(True);
      finally
        EntityList.Free;
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

function TRESTRouter.TryGetEntityRoute(aRequestInfo: TIdHTTPRequestInfo;
  aDBEngine: TDBEngine; out aRespose: string): Boolean;
var
  Entity: TEntityREST;
  ID: Variant;
  LoadParamsClass: TLoadParamsClass;
  Params: TDictionary<string, Variant>;
  RegisteredClass: TClass;
  RESTMessage: TRESTMessage;
  sJSON: string;
  StringList: TStringList;
  TemplateRoute: string;
begin
  Result := False;

  for RegisteredClass in FRegisteredClasses do
  begin
    if not RegisteredClass.InheritsFrom(TEntityAbstract) then
      Continue;

    if TryGetAutoRouteValues(RegisteredClass, {out}TemplateRoute, {out}LoadParamsClass) then
    begin
      Params := TDictionary<string, Variant>.Create;
      try
        if MatchRequestRoute(TemplateRoute, aRequestInfo.URI, Params) then
        begin
          if not Params.TryGetValue('id', {out}ID) then
            Continue;

          Entity := TEntityRESTClass(RegisteredClass).Create(aDBEngine, ID);
          try
            case aRequestInfo.CommandType of
              hcGET:
              begin
                if aRequestInfo.URI.Contains('/isunique/') then
                  aRespose := HandleIsUniqueRequest(Entity, aRequestInfo.URI)
                else
                begin
                  RESTMessage := TRestMessage.Create([Entity]);
                  try
                    aRespose := TJSONSerializer.Serialize<TRESTMessage>(RESTMessage);
                  finally
                    RESTMessage.Free;
                  end;
                end;
              end;

              hcPOST:
              begin
                StringList := TStringList.Create;
                try
                  StringList.Text := aRequestInfo.FormParams;
                  sJSON := StringList.Values['JSON'];
                finally
                  StringList.Free;
                end;

                sJSON := TNetEncoding.URL.Decode(sJSON);
                TJSONSerializer.Deserialize<TRESTMessage>(RESTMessage, sJSON);
                try
                  Entity.UpdateProps(RESTMessage.Instances[0]);
                  Entity.Store;
                finally
                  RESTMessage.Free
                end;

                RESTMessage := TRESTMessage.CreateForID([Entity]);
                try
                  aRespose := TJSONSerializer.Serialize<TRESTMessage>(RESTMessage);
                finally
                  RESTMessage.Free;
                end;
              end;

              hcDELETE:
              begin
                Entity.Delete;
              end;
            end;
          finally
            Entity.Free;
          end;

          Exit(True);
        end;
      finally
        Params.Free;
      end;
    end;
  end;
end;

function TRESTRouter.TryGetRefRouteValues(aLoadParams: TLoadParams;
  aRegistredClass: TClass; out aTemplateRoute: string;
  out aLoadParamsClass: TLoadParamsClass): Boolean;
var
  i: Integer;
  TemplateWords: TArray<string>;
begin
  Result := False;

  if Assigned(aLoadParams) and not(aLoadParams is TRefListLoadParams) then
    Exit;

  if not Assigned(aLoadParams) or (TRefListLoadParams(aLoadParams).ListClass = aRegistredClass) then
  begin
    if TryGetAutoRouteValues(aRegistredClass, {out}aTemplateRoute, {out}aLoadParamsClass) then
    begin
      Result := True;
      aLoadParamsClass := TRefListLoadParams;

      TemplateWords := aTemplateRoute.Split(['/']);
      for i := Length(TemplateWords) - 1 downto 0 do
        if TemplateWords[i].Contains('{') then
          Delete(TemplateWords, i, 1);

      aTemplateRoute := Format('reference/%s/{propname}/{propvalue}', [string.Join('/', TemplateWords)]);
    end;
  end;
end;

{ AutoRoute }

constructor AutoRoute.Create(const aTemplateRoute: string; aLoadParamsClass: TLoadParamsClass);
begin
  FTemplateRoute := aTemplateRoute;
  FLoadParamsClass := aLoadParamsClass;
end;

{ TLoadParams }

function TLoadParams.GetOrderItem: TOrderItem;
begin
  Result := TOrderItem.Create(Order, OrderDirection);
end;

function TLoadParams.PropExists(const aPropName: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Self, aPropName);

  if PropInfo <> nil then
    Result := True
  else
    Result := False;
end;

function TLoadParams.PropValueAsStr(const aPropName: string): string;
var
  Value: Variant;
begin
  Value := GetPropValue(Self, aPropName);
  Result := VarToStr(Value);
end;

{ TJSONSerializer }

class function TJSONSerializer.CreateInstance(aRttiType: TRttiType): TValue;
var
  ConstructorMethod: TRttiMethod;
  ConstructorMethods: TArray<TRttiMethod>;
  RecPointer: Pointer;
begin
  Result := nil;

  if aRttiType.IsInstance then
  begin
    ConstructorMethods := aRttiType.GetMethods('Create');
    for ConstructorMethod in ConstructorMethods do
      if Length(ConstructorMethod.GetParameters) = 0 then
      begin
        Result := ConstructorMethod.Invoke(aRttiType.AsInstance.MetaclassType, []);
        Exit;
      end;

    raise Exception.CreateFmt('TJSONSerializer.CreateInstance: class % must have at least one parameterless constructor', [aRttiType.ToString]);
  end
  else
  if aRttiType.IsRecord then
  begin
    RecPointer := AllocMem(aRttiType.TypeSize);
    TValue.Make(RecPointer, aRttiType.Handle, {var}Result);
    FreeMem(RecPointer);
  end;
end;

class function TJSONSerializer.SerializeStream(const aTypeName: string;
  aStream: TStream): TJSONPair;
var
  Buffer: TBytes;
  sBytes: string;
begin
  if Assigned(aStream) then
  begin
    SetLength(Buffer, aStream.Size);
    aStream.Read(Buffer, 0, aStream.Size);

    sBytes := TNetEncoding.Base64.EncodeBytesToString(Buffer);
  end
  else
    sBytes := '';

  Result := TJSONPair.Create(aTypeName, sBytes);
end;

class procedure TJSONSerializer.Deserialize<T>(var aObject: T; const aJSONString: string);
var
  jsnValue: TJSONValue;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Value: TValue;
begin
  jsnValue := TJSONObject.ParseJSONValue(aJSONString);
  if not Assigned(jsnValue) then
    Exit
  else
  begin
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(TypeInfo(T));

      if jsnValue is TJSONObject then
      begin
        Value := TValue.From<T>(aObject);
        aObject := DeserializeObject(RttiType, {var}Value, jsnValue as TJSONObject).AsType<T>;
      end;
    finally
      jsnValue.Free;
      RttiContext.Free;
    end;
  end;
end;

class function TJSONSerializer.DeserializeArray(aRttiArrayType: TRttiDynamicArrayType;
  aJSON: TJSONArray): TValue;
var
  jsnValue: TJSONValue;
  Value: TValue;
  Values: TArray<TValue>;
begin
  Values := [];

  for jsnValue in aJSON do
  begin
    if jsnValue is TJSONObject then
    begin
      Value := CreateInstance(aRttiArrayType.ElementType);
      DeserializeObject(aRttiArrayType.ElementType, {var}Value, jsnValue as TJSONObject);
    end
    else
    begin
      Value := DeserializeType(aRttiArrayType.ElementType.Name, aRttiArrayType.ElementType, jsnValue);
    end;

    Values := Values + [Value];
  end;

  Result := TValue.FromArray(aRttiArrayType.Handle, Values);
end;

class function TJSONSerializer.DeserializeObject(aRttiType: TRttiType; var aInstance: TValue; aJSON: TJSONObject): TValue;
var
  Field: TRttiField;
  jsnPair: TJSONPair;
  Prop: TRttiProperty;
  Value: TValue;
begin
  for jsnPair in aJSON do
  begin
    for Prop in aRttiType.GetProperties do
    begin
      if Prop.Name = jsnPair.JsonString.Value then
      begin
        Value := DeserializeType(Prop.Name, Prop.PropertyType, jsnPair.JsonValue);
        Prop.SetValue(GetPointer(aInstance), Value);
      end;
    end;

    for Field in aRttiType.GetFields do
    begin
      if Field.Name = jsnPair.JsonString.Value then
      begin
        Value := DeserializeType(Field.Name, Field.FieldType, jsnPair.JsonValue);
        Field.SetValue(GetPointer(aInstance), Value);
      end;
    end;
  end;

  Result := aInstance;
end;

class function TJSONSerializer.DeserializeStream(
  const aJSONValue: string): TStream;
var
  Buffer: TBytes;
begin
  Buffer := TNetEncoding.Base64.DecodeStringToBytes(aJSONValue);

  Result := TMemoryStream.Create;
  Result.Write(Buffer, Length(Buffer));
end;

class function TJSONSerializer.DeserializeType(const aTypeName: string;
  aRttiType: TRttiType; aJSON: TJSONValue): TValue;
begin
  if aJSON is TJSONBool then
    Result := TValue.From<Boolean>(TJSONBool(aJSON).AsBoolean)
  else
  if aJSON is TJSONNumber then
  begin
    if aRttiType.TypeKind = tkInteger then
      Result := TValue.From<Int64>(TJSONNumber(aJSON).AsInt64)
    else
    if aRttiType.TypeKind = tkEnumeration then
      Result := TValue.FromOrdinal(aRttiType.Handle, TJSONNumber(aJSON).AsInt64)
    else
      Result := TValue.From<Double>(TJSONNumber(aJSON).AsDouble);
  end
  else
  if aJSON is TJSONArray then
    Result := DeserializeArray(aRttiType as TRttiDynamicArrayType, aJSON as TJSONArray)
  else
  if aRttiType.IsInstance then
  begin
    if aRttiType.AsInstance.MetaclassType.InheritsFrom(TStream) then
      Result := TValue.From<TStream>(DeserializeStream(aJSON.Value))
  end
  else
  if aJSON is TJSONNull then
    Result := TValue.From<Variant>(Null)
  else
    Result := TValue.From<string>(aJSON.Value);
end;

class function TJSONSerializer.GetPointer(aInstance: TValue): Pointer;
begin
  if aInstance.IsObject then
    Result := aInstance.AsObject
  else
    Result := aInstance.GetReferenceToRawData;
end;

class function TJSONSerializer.GetValue(aRttiMember: TRttiMember; aInstance: TValue): TValue;
begin
  if aRttiMember is TRttiProperty then
    Result := TRttiProperty(aRttiMember).GetValue(GetPointer(aInstance))
  else
  if aRttiMember is TRttiField then
    Result := TRttiField(aRttiMember).GetValue(GetPointer(aInstance))
  else
    Result := nil;
end;

class function TJSONSerializer.Serialize<T>(aObject: T): string;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  jsnValue: TJSONValue;
begin
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(TypeInfo(T));

    case RttiType.TypeKind of
      tkRecord:
      begin
        jsnValue := TJSONObject.Create;
        SerializeObject(RttiType, TValue.From<T>(aObject), jsnValue as TJSONObject);
      end;
    end;

    Result := jsnValue.ToJSON;
    jsnValue.Free;
  finally
    RttiContext.Free;
  end;
end;

class procedure TJSONSerializer.SerializeArray(aRttiArrayType: TRttiDynamicArrayType;
  aArray: TValue; aJSON: TJSONArray);
var
  i: Integer;
  jsnObject: TJSONObject;
  jsnValue: TJSONValue;
begin
  for i := 0 to aArray.GetArrayLength - 1 do
  begin
    if aRttiArrayType.ElementType.IsRecord or aRttiArrayType.ElementType.IsInstance then
    begin
      jsnObject := TJSONObject.Create;
      SerializeObject(aRttiArrayType.ElementType, aArray.GetArrayElement(i), jsnObject);
      aJSON.AddElement(jsnObject);
    end
    else
    begin
      jsnObject := TJSONObject.Create();
      try
        SerializeType(aRttiArrayType.ElementType.Name, aRttiArrayType.ElementType,
          aArray.GetArrayElement(i), jsnObject);

        jsnValue := TJSONObject.ParseJSONValue(jsnObject.Pairs[0].JsonValue.ToJSON) as TJSONValue;

        aJSON.AddElement(jsnValue);
      finally
        jsnObject.Free;
      end;
    end;
  end;
end;

class procedure TJSONSerializer.SerializeObject(aRttiType: TRttiType; aInstance: TValue;
  aJSON: TJSONObject);
var
  Field: TRttiField;
  Prop: TRttiProperty;
  Value: TValue;
begin
  for Prop in aRttiType.GetProperties do
  begin
    Value := GetValue(Prop, aInstance);
    SerializeType(Prop.Name, Prop.PropertyType, Value, aJSON);
  end;

  for Field in aRttiType.GetFields do
  begin
    if not (Field.Visibility in [mvPublic, mvPublished]) then
      Continue;

    Value := GetValue(Field, aInstance);
    SerializeType(Field.Name, Field.FieldType, Value, aJSON);
  end;
end;

class procedure TJSONSerializer.SerializeType(const aTypeName: string;
  aRttiType: TRttiType; aValue: TValue; aJSON: TJSONObject);
var
  jsnArray: TJSONArray;
begin
  if aRttiType.Name.ToUpper = 'BOOLEAN' then
  begin
    aJSON.AddPair(aTypeName, TJSONBool.Create(aValue.AsBoolean));
    Exit;
  end;

  case aRttiType.TypeKind of
    tkInteger:
      aJSON.AddPair(aTypeName, TJSONNumber.Create(aValue.AsInteger));

    tkEnumeration:
      aJSON.AddPair(aTypeName, TJSONNumber.Create(aValue.AsOrdinal));

    tkUString:
      aJSON.AddPair(aTypeName, aValue.AsString);

    tkVariant:
    begin
      if VarIsNull(aValue.AsVariant) then
        aJSON.AddPair(aTypeName, TJSONNull.Create)
      else
        aJSON.AddPair(aTypeName, VarToStr(aValue.AsVariant));
    end;

    tkDynArray:
    begin
      jsnArray := TJSONArray.Create;
      SerializeArray(aRttiType as TRttiDynamicArrayType, aValue, jsnArray);
      aJSON.AddPair(aTypeName, jsnArray);
    end;

    tkClass:
    begin
      if aRttiType.AsInstance.MetaclassType.InheritsFrom(TStream) and (aValue.AsObject <> nil)
      then
        aJSON.AddPair(SerializeStream(aTypeName, aValue.AsType<TStream>));
    end;
  end;
end;

{ TRESTMessage }

constructor TRESTMessage.CreateForID(const aEntities: TArray<TEntityREST>; dummy: Integer);
var
  Entity: TEntityREST;
  Instance: TInstance;
begin
  Init;

  for Entity in aEntities do
  begin
    Instance := TInstance.Create;
    Instance.Fields := [Entity.GetCurrentInstance.GetFieldByName('ID')];

    Instances := Instances + [Instance];
  end;
end;

constructor TRESTMessage.CreateForModifiedFields(
  const aEntities: TArray<TEntityREST>; dummy1: Integer = 0; dummy2: Integer = 0);
var
  Entity: TEntityREST;
  Instance: TInstance;
  LHasModifiedFields: Boolean;
begin
  Init;
  LHasModifiedFields := FHasModifiedFields;

  for Entity in aEntities do
  begin
    Instance := TInstance.Create;
    Instance.Fields := [];

    Entity.GetCurrentInstance.ForEachInstanceField(True{aModifiedOnly},
      procedure(const aInstanceField: TInstanceField; const aPropName: string; const aPropValue: Variant)
      var
        InstanceField: TInstanceField;
      begin
        InstanceField.Init;
        InstanceField.FieldName := aInstanceField.FieldName;
        InstanceField.FieldType := aInstanceField.FieldType;
        InstanceField.Value := aPropValue;

        Instance.Fields := Instance.Fields + [InstanceField];

        if not LHasModifiedFields then
          LHasModifiedFields := True;
      end
    );

    Instances := Instances + [Instance];
    FHasModifiedFields := LHasModifiedFields;
  end;
end;

procedure TRESTMessage.Free;
var
  Instance: TInstance;
begin
  for Instance in Instances do
    Instance.Free;
end;

function TRESTMessage.HasModifiedFields: Boolean;
begin
  Result := FHasModifiedFields;
end;

constructor TRESTMessage.Create(const aEntities: TArray<TEntityREST>);
var
  Entity: TEntityREST;
  Instance: TInstance;
begin
  Init;

  for Entity in aEntities do
  begin
    Instance := TInstance.Create;
    Instance.Fields := Entity.GetCurrentInstance.Fields;

    Instances := Instances + [Instance];
  end;
end;

procedure TRESTMessage.Init;
begin
  FHasModifiedFields := False;
  CanLoadMore := False;
  NextRecNum := 0;
  Instances := [];
  Data := [];
end;

{ TEntityREST }

function TEntityREST.CheckIsValueUnique(const aPropName: string;
  const aValue: Variant): Boolean;
var
  RESTMessage: TRESTMessage;
  sResponse: string;
begin
  if Assigned(FRESTClient) then
  begin
    sResponse := FRESTClient.Get(GetCheckIsValueUniqueRoute(aPropName, aValue));

    TJSONSerializer.Deserialize<TRESTMessage>({var}RESTMessage, sResponse);

    Result := RESTMessage.Data[0];
  end
  else
    Result := inherited CheckIsValueUnique(aPropName, aValue);
end;

constructor TEntityREST.Create(aRESTClient: TRESTClient; const aID: Integer);
begin
  FRESTClient := aRESTClient;

  inherited Create(nil{aDBEngine}, aID);
end;

procedure TEntityREST.DeleteToDB;
begin
  if Assigned(FRESTClient) then
    FRESTClient.Delete(TRESTRouter.EncodeID(Self))
  else
    inherited DeleteToDB;
end;

function TEntityREST.GetCheckIsValueUniqueRoute(const aPropName: string;
  const aValue: Variant): string;
begin
  Result := TRESTRouter.EncodeID(Self) + Format('/isunique/%s/%s', [aPropName, VarToStr(aValue)]);
end;

function TEntityREST.GetCurrentInstance: TInstance;
var
  InstanceField: TInstanceField;
  PropName: string;
  PublishedProps: TArray<string>;
begin
  if Assigned(FInstance) then
    Result := FInstance
  else
  begin
    FInstance := TInstance.Create;
    FInstance.SetEntity(Self);

    PublishedProps := TORMTools.GetPublishedProps(ClassType);
    for PropName in PublishedProps do
    begin
      InstanceField.Init;
      InstanceField.FieldName := TORMTools.GetFieldNameByPropName(PropName);
      InstanceField.Value := Prop[PropName];

      FInstance.Fields := FInstance.Fields + [InstanceField];
    end;

    Result := FInstance;
  end;
end;

procedure TEntityREST.InsertToDB;
var
  RESTMessage: TRESTMessage;
  sRequestJSON: string;
  sResponseJSON: string;
begin
  if Assigned(FRESTClient) then
  begin
    RESTMessage := TRESTMessage.Create([Self]);
    try
      sRequestJSON := TJSONSerializer.Serialize<TRESTMessage>(RESTMessage);
      sResponseJSON := FRESTClient.Post(TRESTRouter.EncodeID(Self), ['JSON'], [sRequestJSON]);
    finally
      RESTMessage.Free;
    end;

    TJSONSerializer.Deserialize<TRESTMessage>(RESTMessage, sResponseJSON);
    try
      ID := RESTMessage.Instances[0].GetFieldByName('ID').Value;
    finally
      RESTMessage.Free;
    end;
  end
  else
    inherited InsertToDB;
end;

procedure TEntityREST.UpdateProps(aInstance: TInstance);
begin
  AssignPropsFromInstance(aInstance);
end;

procedure TEntityREST.UpdateToDB;
var
  RESTMessage: TRESTMessage;
  sJSON: string;
begin
  if Assigned(FRESTClient) then
  begin
    RESTMessage := TRESTMessage.CreateForModifiedFields([Self]);
    try
      if RESTMessage.HasModifiedFields then
      begin
        sJSON := TJSONSerializer.Serialize<TRESTMessage>(RESTMessage);

        FRESTClient.Post(gRESTRouter.EncodeID(Self), ['JSON'], [sJSON]);
        AssignInstanceFromProps;
      end;
    finally
      RESTMessage.Free;
    end;
  end
  else
    inherited UpdateToDB;
end;

initialization
  gRESTRouter := TRESTRouter.Create;

finalization
  gRESTRouter.Free;

end.
