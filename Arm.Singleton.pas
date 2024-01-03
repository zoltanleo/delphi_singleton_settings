unit Arm.Singleton;

interface
uses
  System.Contnrs;

type
  {from here https://ins911.blogspot.com/2008/12/singletone-delphi.html}
  // Базовый класс для объектов, реализующих паттерн
  // "Singleton". Для получения доступа к экземпляру
  // необходимо вызвать GetInstance. Если экземпляр
  // еще не существует, то он будет создан. Иначе -
  // возвращена ссылка на ранее созданный экземпляр.
  // Уничтожить экземпляр можно вручную, вызвав Free,
  // иначе он будет уничтожен автоматически перед
  // завершением приложения

  TSingleton = class(TObject)
  private
    class procedure RegisterInstance(Instance: TSingleton);
    procedure UnRegisterInstance;
    class function FindInstance: TSingleton;
  protected
    // Инициализацию производить только в этом
    // конструкторе, а не в GetInstance.
    // Не рекомендуется выносить этот конструктор
    // из секции protected
    constructor Create; virtual;
  public
    class function NewInstance: TObject; override;
    procedure BeforeDestruction; override;
    // Точка доступа к экземпляру
    constructor GetInstance;
  end;

implementation

var
  SingletonList: TObjectList;

{ TSingleton }

procedure TSingleton.BeforeDestruction;
begin
  UnregisterInstance;
  inherited BeforeDestruction;
end;

constructor TSingleton.Create;
begin
  inherited Create;
end;

class function TSingleton.FindInstance: TSingleton;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SingletonList.Count - 1 do
    if SingletonList[i].ClassType = Self then
    begin
      Result := TSingleton(SingletonList[i]);
      Break;
    end;
end;

constructor TSingleton.GetInstance;
begin
  inherited Create;
end;

class function TSingleton.NewInstance: TObject;
begin
  Result := FindInstance;
  if Result = nil then
  begin
    Result := inherited NewInstance;
    TSingleton(Result).Create;
    RegisterInstance(TSingleton(Result));
  end;
end;

class procedure TSingleton.RegisterInstance(Instance: TSingleton);
begin
  SingletonList.Add(Instance);
end;

procedure TSingleton.UnRegisterInstance;
begin
  SingletonList.Extract(Self);
end;

initialization
    SingletonList := TObjectList.Create(True);

finalization
    SingletonList.Free;
end.
