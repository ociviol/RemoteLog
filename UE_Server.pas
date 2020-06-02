unit UE_Server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  // synapse
  blcksock;

type

  { TUEServerThread }

  TUEServerThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TUEServer = class
  private
    FUEServerThread: TUEServerThread;
    function GetRunning: Boolean;
    procedure SetEnabled(AValue: Boolean);
  public
    procedure Stop;
    procedure Start;
    property Running: Boolean read GetRunning;
    property Enabled : Boolean read GetRunning write SetEnabled;
  end;

implementation

{ TUEServer }

function TUEServer.GetRunning: Boolean;
begin
  Result := FUEServerThread <> nil;
end;

procedure TUEServer.SetEnabled(AValue: Boolean);
begin
  if Assigned(FUEServerThread) then
    Stop
  else
    Start;
end;

procedure TUEServer.Start;
begin
  FUEServerThread := TUEServerThread.Create(False);
end;

procedure TUEServer.Stop;
begin
  if FUEServerThread <> nil then
  begin
    FUEServerThread.Terminate;
    FUEServerThread.WaitFor;
    FreeAndNil(FUEServerThread);
  end;
end;

{ TUEServerThread }

procedure TUEServerThread.Execute;
var
  Socket: TUDPBlockSocket;
  Buffer: string;
  //Size: Integer;
begin
  Socket := TUDPBlockSocket.Create;
  try
    Socket.Bind('0.0.0.0', '7');
    try
      if Socket.LastError <> 0 then
      begin
        raise Exception.CreateFmt('Bind failed with error code %d', [Socket.LastError]);
        Exit;
      end;

      while not Terminated do
      begin
        // wait one second for new packet
        Buffer := Socket.RecvPacket(1000);

        if Socket.LastError = 0 then
        begin
          // just send the same packet back
          Socket.SendString(Buffer);
        end;

        // minimal sleep
        if Buffer = '' then
          Sleep(10);
      end;

    finally
      Socket.CloseSocket;
    end;
  finally
    Socket.Free;
  end;
end;

end.
