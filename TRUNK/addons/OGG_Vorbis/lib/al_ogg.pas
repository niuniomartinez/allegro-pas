unit al_ogg;
(* Adds OGG/Vorbis file samples to Allegro.
 *)

interface

uses
  allegro, albase;


(* OGG/Vorbis loader.  You can call it directly or regiter it using
  al_register_sample_file_type ('ogg', @LoadOGGVorbis, NIL); *)
  function LoadOGGVorbis (filename: AL_STRptr): AL_SAMPLEptr; CDECL;



implementation

uses
  SysUtils,
  classes,
  VorbisFile;

type
  BufferPointer = ^BufferUnit;      {Pointer at buffer unit}
  BufferUnit = record
                    Data : AL_STRptr;        {Decoded sample data}
                    Length : AL_INT;         {Number of bytes}
                    Next : BufferPointer;    {Next buffer unit}
                  end;


{Loads an OGG Vorbis}

function LoadOGGVorbis (filename: AL_STRptr): AL_SAMPLEptr; CDECL;
var NewSample : AL_SAMPLEptr;        {New sample structure}
    FileStream : TFileStream;        {File stream}
    VorbisStream  : OggVorbis_File;  {Vorbis stream}
    Length : longint;                {Length of decoded data}
    Buffer : BufferPointer;          {Decoded samples}
    Position : BufferPointer;        {Current position in buffer}
    Bitstream : integer;             {Vorbis bitstream position}
begin
  NewSample := nil;
  if not FileExists(filename) then
  begin
    LoadOGGVorbis := NewSample;
    exit;
  end;

{Open Vorbis}
  FileStream := TFileStream.Create(filename, fmOpenRead);
  if ov_open_callbacks(FileStream, VorbisStream, nil, 0, ops_callbacks) = 0 then
  begin
    if VorbisStream.vi^.channels <= 2 then
    begin
      new(Buffer);
      Position := Buffer;
      Position^.Next := nil;

    {Decode samples into buffer}
      GetMem(Position^.Data, 4096);
      Length := ov_read(VorbisStream, PByteArray(Position^.Data)^[0], 4096, 0, 2, 0, @Bitstream);
      while Length > 0 do
      begin
        Position^.Length := Length;
        new(Position^.Next);
        Position := Position^.Next;
        Position^.Next := nil;
        GetMem(Position^.Data, 4096);

        Length := ov_read(VorbisStream, PByteArray(Position^.Data)^[0], 4096, 0, 2, 0, @Bitstream);
      end;
      Position^.Length := 0;

      if Length >= 0 then
      begin
      {Calculate total length}
        Length := 0;
        Position := Buffer;
        repeat
          Length := Length + Position^.Length;
          Position := Position^.Next;
        until Position = nil;
        Length := Length div (2 * VorbisStream.vi^.channels);

      {Convert into sample structure}
        NewSample := al_create_sample(16, VorbisStream.vi^.channels, VorbisStream.vi^.rate, Length);
        if NewSample <> nil then
        begin
          Length := 0;
          Position := Buffer;
          while Position^.Next <> nil do
          begin
            Move(PByteArray(Position^.Data)^[0], PByteArray(NewSample^.data)^[Length], Position^.Length);
            Length := Length + Position^.Length;
            Position := Position^.Next;
          end;
        end;

      {Free resources}
        while Buffer <> nil do
        begin
          FreeMem(Buffer^.Data, 4096);
          Position := Buffer;
          Buffer := Buffer^.Next;
          dispose(Position);
        end;
      end;
    end;
    ov_clear(VorbisStream);
  end;

  LoadOGGVorbis := NewSample;
end;

end.
