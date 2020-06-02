unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SynEdit, UE_Server;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    ToggleBox1: TToggleBox;
    procedure FormCreate(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
  private
    FUEServer : TUEServer;
  public

  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

uses
  strutils;

{ TMainFrm }

procedure TMainFrm.ToggleBox1Change(Sender: TObject);
begin
  ToggleBox1.Caption:= ifthen(ToggleBox1.State = cbChecked, 'Stop', 'Start') + ' Server';
  FUEServer.Enabled := ToggleBox1.State = cbChecked;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  FUEServer := TUEServer.Create;
end;

end.

