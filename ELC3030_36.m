%============================================
% read configation file
%============================================
fileName = 'config.json';       % filename in JSON extension
fid = fopen(fileName);          % Opening the file
raw = fread(fid,inf);              % Reading the contents
str = char(raw');                   % Transformation
fclose(fid);                           % Closing the file
data = jsondecode(str);        % Using the jsondecode function to parse JSON from string
%============================================
% Check The   DATA
%============================================
if (data(1).protocol_name=="UART" && data(2).protocol_name=="USB")
    URATIndex=1;
    USBIndex=2;
elseif (data(1).protocol_name=="USB" && data(2).protocol_name=="UART")
    URATIndex=2;
    USBIndex=1;
else
    disp("Error in Config File");
end

URATdata=data(URATIndex);
USBdata=data(USBIndex);
%============================================
% Create Gloabl Variables
%============================================
UART_data_bits=URATdata.parameters.data_bits;
UART_stop_bits=URATdata.parameters.stop_bits;
UART_parity=URATdata.parameters.parity;
UART_bit_duration=URATdata.parameters.bit_duration;

USB_sync_pattern=USBdata.parameters.sync_pattern;
USB_pid=USBdata.parameters.pid;
USB_dest_address=USBdata.parameters.dest_address;
USB_payload=USBdata.parameters.payload;
USB_bit_duration=USBdata.parameters.bit_duration;
USB_SyncPaternLength=size(USB_sync_pattern);
USB_SyncPaternLength=USB_SyncPaternLength(1,2);
USB_AddressLength=size(USB_dest_address);
USB_AddressLength=USB_AddressLength(1,2);

%============================================
% Read Input File
%============================================
    fileInputName = 'input.txt';                % filename in Text extension
    fInputid = fopen(fileInputName);       % Opening the file
    rawInput = fread(fInputid,inf);           % Reading the contents
    fclose(fInputid);                                % Closing the file

%============================================
% UART Program Part
%============================================
    UART_BinaryInput=dec2bin(rawInput,UART_data_bits);      %convirt Input Data to binary 
    UART_USB_TransposedBinaryInput= fliplr(UART_BinaryInput);   %Flipped  Binary Input

    % generate matrix dimention
     UART_columnsMatrixSize=1+UART_data_bits+UART_stop_bits;
     if(UART_parity~="none")
         UART_columnsMatrixSize=UART_columnsMatrixSize+1;      %Caclulate Colums Size
     end

     %Caclulate rows Size
     UART_rowMatrixSize=size(rawInput);
     UART_rowMatrixSize=UART_rowMatrixSize(1,1);

    % inialize matrix for all data [start +data + parity +stop]
   UART_AllDataMatrix=zeros(UART_rowMatrixSize,UART_columnsMatrixSize);

    for i = 1:UART_rowMatrixSize

           UART_AllDataMatrix(i,1)=0;       % start bit at first

           % add data bits
           for x = 2:UART_data_bits+1
           UART_AllDataMatrix(i,x)=UART_USB_TransposedBinaryInput(i,x-1)-48;
           end

           % Check the parity
           numOfOnes=0;
           for k = 1:UART_data_bits+1
               if(UART_AllDataMatrix(i,k)==1)
                   numOfOnes=numOfOnes+1;
               end
           end

           if(UART_parity=="odd"&&mod(numOfOnes,2)==0)       % parity is odd and ones is even
               UART_AllDataMatrix(i,UART_data_bits+2)=1;
               
           elseif(UART_parity=="odd"&&mod(numOfOnes,2)~=0)   % parity is odd and ones is odd
               UART_AllDataMatrix(i,UART_data_bits+2)=0;
               
           elseif(UART_parity=="even"&&mod(numOfOnes,2)~=0)  % parity is even and ones is odd
               UART_AllDataMatrix(i,UART_data_bits+2)=1;
               
           elseif(UART_parity=="even"&&mod(numOfOnes,2)==0)  % parity is even and ones is even
               UART_AllDataMatrix(i,UART_data_bits+2)=0;
           end

          % add end bits
          if(UART_stop_bits==1)
               UART_AllDataMatrix(i,UART_columnsMatrixSize)=1;
          else
              UART_AllDataMatrix(i,UART_columnsMatrixSize)=1;
              UART_AllDataMatrix(i,UART_columnsMatrixSize-1)=1;
          end

    end  % End of Main Loop For UART
    
    
%============================================
% First 2 bytes in UART Part
%============================================
%      %plot first two bytes
%      bitstream =cat(2, UART_AllDataMatrix(1,:),UART_AllDataMatrix(2,:));      % create random stream of zeros and ones (binary)
%      stairs(bitstream);                                                                                 % This makes the bitstream visible
%      ylim([-1 2]);                                                                                         % Add some clearance above and below
%      xlim([0,2*UART_columnsMatrixSize+1])
%  
     


%============================================
% USB Program Part
%============================================
USB_BinaryInput=dec2bin(rawInput,8);                             %convirt USB Input Data to binary
USB_TransposedBinaryInput= fliplr(USB_BinaryInput);     %Flipped  USB Input Data Binary Input

% generate matrix dimention
 USB_columnsMatrixSize=USB_SyncPaternLength+USB_pid+USB_AddressLength+USB_payload*8+2;        %cacl Colums Size
 USB_rowMatrixSize=size(rawInput);       %cacl rows Size

  % numer of bits which not complete packet
  remiderBytes=mod(USB_rowMatrixSize,USB_payload);
  remiderBytes=remiderBytes(1,1);
  USB_rowMatrixSize=ceil(USB_rowMatrixSize(1,1)/USB_payload);
  
    % inialize matrix for all data [NumSyncPatern+Pid+AddressLengh+Payload*8+2]
    USB_AllDataMatrix=zeros(USB_rowMatrixSize,USB_columnsMatrixSize);

PidCounter=1;
  % FileSizeCase =0   if  File Size Bytes is Divisable by 128 
  % FileSizeCase =1   if  File Size Bytes  >128
  % FileSizeCase =2   if  File Size Bytes  <128
FileSizeCase=0;
if(USB_rowMatrixSize>0 &&remiderBytes==0 )
    FileSizeCase=0;
elseif(USB_rowMatrixSize~=1 &&remiderBytes~=0 )
    FileSizeCase=1;
 elseif(USB_rowMatrixSize==1 &&remiderBytes~=0 )
    FileSizeCase=2;
end

    for i = 1:USB_rowMatrixSize

        % start of Pattern (Sync Pattern)
            for k=1:USB_SyncPaternLength
                 USB_AllDataMatrix(i,k)=USB_sync_pattern(1,k)-48;
            end

        %   Pid_Pattern
        TransposedPidCounter=fliplr(dec2bin(PidCounter,4));
        CompPid=bitcmp(PidCounter,'uint8');
        TransposedPidCounterComplement=fliplr(dec2bin(CompPid,8));
        TotalPid=cat(2,TransposedPidCounter,TransposedPidCounterComplement(1:4));
           for k=1:USB_pid
                 USB_AllDataMatrix(i,k+USB_SyncPaternLength)=TotalPid(1,k)-48;
            end

         PidCounter=PidCounter+1;
         if(PidCounter==15)
                PidCounter=0;
         end

       % Transmitting Address

       for k=1:USB_AddressLength
             USB_AllDataMatrix(i,k+USB_SyncPaternLength+USB_pid)=USB_dest_address(1,k)-48;
       end

      if(FileSizeCase==0 || (FileSizeCase==1 && i<USB_rowMatrixSize) )
      %Transmitting the data Packet
        s=USB_SyncPaternLength+USB_pid+USB_AddressLength;
        Bit_Num=s+1;
           for Byte_num =((USB_payload)*(i-1)+1):(USB_payload)*i
               for k=1:8
             USB_AllDataMatrix(i,Bit_Num)=USB_TransposedBinaryInput(Byte_num,k)-48;
             Bit_Num=Bit_Num+1;
               end
           end
      else  % Add Reminder Data
            s=USB_SyncPaternLength+USB_pid+USB_AddressLength;
            Bit_Num=s+1;
           for Byte_num =((USB_payload)*(i-1)+1):(((USB_payload)*(i-1)+1)+remiderBytes-1)
               for k=1:8
                    USB_AllDataMatrix(i,Bit_Num)=USB_TransposedBinaryInput(Byte_num,k)-48;
                    Bit_Num=Bit_Num+1;
               end
           end

      end


    end  % end of USB main foor loop

%============================================
% Make BitStuffing   
%============================================
for L=1:USB_rowMatrixSize
     vector{L}=USB_AllDataMatrix(L,:); %make every row in separate Vector
     vectorSize=USB_columnsMatrixSize;
     onesCounter=0;
     for k=1:vectorSize
         if(vector{L}(1,k)==1)
             onesCounter=onesCounter+1;               
             if(onesCounter==6)
               onesCounter=0;
               vector{L}=[vector{L}(1:k) 0 vector{L}(k+1:vectorSize)];
               vectorSize=vectorSize+1;
             end
         else
              onesCounter=0;
        end
    end
end
%============================================
%Make NON Zero Inversion
%============================================
USB_NZIDataMatrix=USB_AllDataMatrix;
for rI =1:USB_rowMatrixSize
                NZICState=1;  % Idel State
               for cI=1:USB_columnsMatrixSize-2
                     if(USB_AllDataMatrix(rI,cI)==0)
                         USB_NZIDataMatrix(rI,cI)=~NZICState;
                         NZICState= USB_NZIDataMatrix(rI,cI);
                     else
                         USB_NZIDataMatrix(rI,cI)=NZICState;
                          NZICState= USB_NZIDataMatrix(rI,cI);
                     end
               end
end
%============================================
%Make NON Zero Inversion Defrential
%============================================
USB_NZRIDataMatrix=~USB_NZIDataMatrix;
% Reset 2  stop bits
for rI =1:USB_rowMatrixSize
         for cI=USB_columnsMatrixSize-1:USB_columnsMatrixSize
              USB_NZRIDataMatrix(rI,cI)=0;      
         end
end

%============================================
%  Plot First 2 Packet in USB Part
%============================================
%     USB_bitstream =cat(2, USB_NZIDataMatrix(1,:),USB_NZIDataMatrix(2,:)) + 1;      % create random stream of zeros and ones (binary)
%     stairs(USB_bitstream);                                                       % This makes the bitstream visible
%     ylim([-2 3]);                                                                          % Add some clearance above and below
%     xlim([1,16]);
%     
%     hold on  
%     
%     % For First 2 bytes in USB Part
%     USB2_bitstream =cat(2, USB_NZRIDataMatrix(1,:),USB_NZRIDataMatrix(2,:))-1;      % create random stream of zeros and ones (binary)
%     stairs(USB2_bitstream);                                                       % This makes the bitstream visible
%     ylim([-2 3]);                                                                % Add some clearance above and below
%     xlim([1,16]);
%     hold off  

%============================================
%Make Final Calculation 
%============================================
if(UART_bit_duration==USB_bit_duration)
    
    %============================================
    %Make UART Calculation 
    %============================================
    
    %total time required to transmit the input data file,
    % TimeTotal = numOfBits * BitDuration
    UART_TimeTotal=(UART_columnsMatrixSize*UART_rowMatrixSize)*UART_bit_duration;
    disp("UART_TimeTotal = "+UART_TimeTotal+ "sec");

    %percentage overhead= numOfNonDataBits/totalBits
    UART_percentageOverhead=(UART_rowMatrixSize*(UART_columnsMatrixSize-UART_data_bits)) /(UART_columnsMatrixSize*UART_rowMatrixSize) *100;
    disp("UART_percentageOverhead = "+UART_percentageOverhead +"%");

    %efficiency = numOfDatabits/totalBits
    UART_efficiency=(UART_rowMatrixSize*UART_data_bits) /(UART_columnsMatrixSize*UART_rowMatrixSize) *100;
    disp("UART_efficiency = "+UART_efficiency +"%");

    % Ploting percentage overhead versus file size
    
     fileSize_Axis=[size(rawInput,1),size(rawInput,1) * 2,size(rawInput,1) * 3,size(rawInput,1)*4 ,size(rawInput,1) * 5];
     
      UART_perOverheadAxis=[ UART_percentageOverhead, UART_percentageOverhead,UART_percentageOverhead,UART_percentageOverhead,UART_percentageOverhead];
%       plot(fileSize_Axis,UART_perOverheadAxis);
% 
%     %  transmission versus the file size
%     % fileSize=[20,40,60,80,100];
         UART_transTime = UART_bit_duration*UART_columnsMatrixSize*UART_rowMatrixSize;
         UART_transTimeAxis=[ UART_transTime, UART_transTime*2, UART_transTime*3, UART_transTime*4, UART_transTime*5];
%        plot(fileSize_Axis,UART_transTimeAxis);

%============================================
%Make USB Calculation 
%============================================
%total time required to transmit the input data file,
    % TimeTotal = numOfBits * BitDuration
    USB_TimeTotal=(USB_columnsMatrixSize*USB_rowMatrixSize)*USB_bit_duration;
    disp("USB_TimeTotal = "+USB_TimeTotal+ "sec");

    %percentage overhead= numOfNonDataBits/totalBits
    if (FileSizeCase == 0)	%File Size Bytes is Divisable by 128 
        USB_percentageOverhead=(USB_rowMatrixSize*(USB_columnsMatrixSize-USB_payload * 8)) /(USB_columnsMatrixSize*USB_rowMatrixSize) *100;
        USB_efficiency=(USB_rowMatrixSize*USB_payload * 8) /(USB_columnsMatrixSize*USB_rowMatrixSize) *100;
   
   elseif (FileSizeCase==1)	% File Size Bytes  >128
   
        USB_percentageOverhead=((USB_rowMatrixSize-1)*(USB_columnsMatrixSize-USB_payload * 8)+(USB_columnsMatrixSize - remiderBytes*8)) /(USB_columnsMatrixSize*USB_rowMatrixSize) *100;
        USB_efficiency=((USB_rowMatrixSize - 1)*(USB_payload * 8)+remiderBytes*8) /(USB_columnsMatrixSize*USB_rowMatrixSize) *100;
   
   elseif (FileSizeCase==2)  %File Size Bytes  <128
        USB_percentageOverhead=(USB_columnsMatrixSize - remiderBytes*8) /(USB_columnsMatrixSize*USB_rowMatrixSize) *100;
         USB_efficiency=(remiderBytes*8) /(USB_columnsMatrixSize*USB_rowMatrixSize) *100;
    end
    disp("USB_percentageOverhead = "+USB_percentageOverhead +"%");

    %efficiency = numOfDatabits/totalBits
    disp("USB_efficiency = "+USB_efficiency +"%");
    
    
	
	 fileSize_Axis=[size(rawInput,1),size(rawInput,1) * 2,size(rawInput,1) * 3,size(rawInput,1)*4 ,size(rawInput,1) * 5];
	% Ploting percentage overhead versus file size
	USB_perOverheadCalc = zeros(1,5);
	USB_transTimeCalc = zeros(1,5);
 for V=1 : 5                                                 %calculating the type of the input file after increasing the size
                                                  % V -> File version

  FileRows=size(rawInput) * V;       %cacl rows Size

  % numer of bits which not complete packet
  remiderBytes=mod(FileRows,USB_payload);
  remiderBytes=remiderBytes(1,1);
  FileRows=ceil(FileRows(1,1)/USB_payload);
  
  % FileSizeCase =0   if  File Size Bytes is Divisable by 128 
  % FileSizeCase =1   if  File Size Bytes  >128
  % FileSizeCase =2   if  File Size Bytes  <128

  USB_transTimeCalc(1,V)=(USB_columnsMatrixSize*FileRows)*USB_bit_duration;
if(FileRows>0 &&remiderBytes==0 )
    USB_perOverheadCalc(1,V) = ( FileRows *(USB_columnsMatrixSize-USB_payload * 8)) /(USB_columnsMatrixSize* FileRows) *100;
elseif(FileRows~=1 &&remiderBytes~=0 )
    USB_perOverheadCalc(1,V) = (( FileRows-1)*(USB_columnsMatrixSize-USB_payload * 8)+(USB_columnsMatrixSize - remiderBytes*8)) /(USB_columnsMatrixSize* FileRows) *100;
 elseif(FileRows==1 &&remiderBytes~=0 )
    USB_perOverheadCalc(1,V) =(USB_columnsMatrixSize - remiderBytes*8) /(USB_columnsMatrixSize* FileRows) *100;
end

 
  end    
    
          %  plot(fileSize_Axis,USB_perOverheadCalc);
	       plot(fileSize_Axis,USB_transTimeCalc);
%============================================
%Generate Output File
%============================================
  
UART_Struct = struct();


UART_Struct.Protocolname = 'UART';
UART_Struct.outputs.total_tx_time = UART_TimeTotal;
UART_Struct.outputs.overhead =UART_percentageOverhead ;
UART_Struct.outputs.efficiency = UART_efficiency;

USB_Struct  = struct();
USB_Struct.Protocolname = 'USB';
USB_Struct.outputs.total_tx_time = USB_TimeTotal;
USB_Struct.outputs.overhead = USB_percentageOverhead;
USB_Struct.outputs.efficiency =USB_efficiency;

OUT=fopen('output.json','w');

fprintf(OUT,'%s','[' );
UART_string=jsonencode(UART_Struct);

 UART_string = strrep(UART_string, "{", "{\n \t");
 UART_string = strrep(UART_string, ',', ',\n\t\t');
 UART_string = strrep(UART_string, "}", "\n \t}");
fprintf(OUT,UART_string);

fprintf(OUT,'%s\n',',');
USB_string=jsonencode(USB_Struct);
 USB_string = strrep(USB_string, "{", "{\n \t");
 USB_string = strrep(USB_string, ',', ',\n\t\t');
 USB_string = strrep(USB_string, "}", "\n \t}");
 
fprintf(OUT,USB_string);
fprintf(OUT,'%s',']');
fclose(OUT);
%============================================

end


    