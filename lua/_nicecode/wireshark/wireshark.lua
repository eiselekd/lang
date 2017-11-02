--  from http://torsten-traenkner.de/linux/development/wireshark.php
--
-- Example Protocol Wireshark dissector (a.k.a. decoder)
-- Author: Torsten Traenkner
-- Version History:
-- 0.01 (02.04.2015)
--
-- This dissector decodes an example protocol.
--
-- use with:
-- wireshark -Xlua_script:example.lua example.pcap
--

-- #############################
-- ## DECODER SETTINGS BEGIN: ##
-- #############################

print("[+] Start");

debug_example_protocol = true

-- add your TCP ports here:
example_tcp_ports = {
  9000, 9001
}

-- display example layers as tree info in wireshark

example_add_tree_info = true

-- ##########################
-- ## DECODER SETTINGS END ##
-- ##########################


do

  local example_layer_2_message_types = {
    [0x11]  = "Connect Request",
    [0x12]  = "Connect Response",
    [0x13]  = "Data",
    [0x14]  = "Disconnect"
  };

  local example_layer_1_message_types = {
    [1]  = "Connect Request",
    [2]  = "Connect Response",
    [3]  = "Data",
    [4]  = "Disconnect"
  };

  local example_tree = 0

  function append_text_to_example_tree(text, packet_info)
    packet_info.cols.info:append(text)
    example_tree:append_text(text)
  end


  -- #####################
  -- ## Example Layer 2 ##
  -- #####################

  print("[+] Add example_layer_2");
  example_layer_2 = Proto("example_layer_2", "Example Layer 2")

  local example_layer_2_fields = example_layer_2.fields
  example_layer_2_fields.message_type = ProtoField.uint16("example_layer_2_fields.message_type", "Message Type", base.HEX)
  example_layer_2_fields.length = ProtoField.uint16("example_layer_2_fields.length", "Length", base.DEC)
  example_layer_2_fields.userdata = ProtoField.bytes("example_layer_2_fields.userdata", "Userdata", base.HEX)

  function example_layer_2.dissector(buffer, packet_info, tree)

    if buffer:len() < 4 then return end

    -- message type
    local example_layer_2_message_type = buffer(0, 2)
    local message_type = "unknown"
    if example_layer_2_message_types[example_layer_2_message_type:uint()] ~= nil then
      message_type = example_layer_2_message_types[example_layer_2_message_type:uint()]
    end

    local ascii = ""

    -- add layer info to protocol tree
    if example_add_tree_info then

      -- create subtree for example layer 2
      example_layer_2_tree = tree:add(example_layer_2, buffer(0))

      -- message type
      local treeitem = example_layer_2_tree:add(example_layer_2_fields.message_type, example_layer_2_message_type)
      treeitem:set_text("Message Type: " .. message_type)

      -- length
      example_layer_2_tree:add(example_layer_2_fields.length,buffer(2, 2))

      print("layer 2 length " .. buffer:len())

      if buffer:len() > 4 then
        -- user data
        example_layer_2_tree:add(example_layer_2_fields.userdata,buffer(4, buffer:len() - 4))

        -- convert hexadecimal string to ASCII string
        start = 4
        endPosition = buffer:len() - 1
        for index = start, endPosition do
          local c = buffer(index,1):uint()

          -- append printable characters
          if c >= 0x20 and c <= 0x7E then
            ascii = ascii .. string.format("%c", c)
          else
            -- use a dot for the others bytes
            ascii = ascii .. "."
          end
        end

      end

    end

    if message_type ~= "Data" then
      append_text_to_example_tree(" Layer 2 [" .. message_type .. "]", packet_info)
    else
      append_text_to_example_tree(" Layer 2 [" .. message_type .. ": " .. ascii .. "]", packet_info)
    end

  end


  -- #####################
  -- ## Example Layer 1 ##
  -- #####################

  print("[+] Add example_layer_1");
  example_layer_1 = Proto("example_layer_1", "Example Layer 1")

  local example_layer_1_fields = example_layer_1.fields
  example_layer_1_fields.message_type = ProtoField.uint16("example_layer_1_fields.message_type", "Message Type", base.HEX)
  example_layer_1_fields.length = ProtoField.uint16("example_layer_1_fields.length", "Length", base.DEC)

  function example_layer_1.dissector(buffer, packet_info, tree)

    if buffer:len() < 4 then return end

    -- message type
    local example_layer_1_message_type = buffer(0, 2)
    local message_type = "unknown"
    if example_layer_1_message_types[example_layer_1_message_type:uint()] ~= nil then
      message_type = example_layer_1_message_types[example_layer_1_message_type:uint()]
    end

    -- add layer info to protocol tree
    if example_add_tree_info then

      -- create subtree for example layer 1
      example_layer_1_tree = tree:add(example_layer_1, buffer(0))

      -- message type
      local treeitem = example_layer_1_tree:add(example_layer_1_fields.message_type, example_layer_1_message_type)
      treeitem:set_text("Message Type: " .. message_type)

      -- length
      example_layer_1_tree:add(example_layer_1_fields.length,buffer(2, 2))

    end

    if message_type ~= "Data" then
      append_text_to_example_tree(" [" .. message_type .. "]", packet_info)
    end

    Dissector.get("example_layer_2"):call(buffer(4, buffer:len() - 4):tvb(), packet_info, tree)
  end


  -- #########################################
  -- ## example protocol all layers chained ##
  -- #########################################

  print("[+] Add example");
  example_protocol = Proto("example", "Example Protocol")

  -- Example Protocol dissector definition
  --   buffer contains the packet data
  --   packet_info is packet info
  --   tree is root of the Wireshark tree view
  function example_protocol.dissector(buffer, packet_info, tree)

    print("decoding example protocol " .. buffer:len())

    local length = 0
    local packet_offset = 0
    packet_info.cols.info = tostring(packet_info.src_port) .. " ----> " .. tostring(packet_info.dst_port)

    -- decode in a loop since one tcp packet can contain several example protocol packets
    -- also reassemble splitted packets
    repeat

      -- check if there are enough bytes for the length field
      if buffer:len() - length >= 4 then
        -- the buffer contains the length field

        -- read the length field
        local packet_length = buffer(length + 2, 2):uint() + 4
        length = length + packet_length

        -- check for complete packet
        if length > buffer:len() then
          -- not all data available yet
          packet_info.desegment_len = length - buffer:len()
          return
        end

        -- real dissector starts here with complete packets

        packet_info.cols.protocol = example_protocol.name
        packet_info.cols.info:append(",")

        -- create subtree for exaple protocol
        example_tree = tree:add(example_protocol, buffer(packet_offset, packet_length))

        Dissector.get("example_layer_1"):call(buffer(packet_offset, packet_length):tvb(), packet_info, example_tree)

        packet_offset = packet_offset + packet_length

      else
        -- the buffer does not contain the length field
        packet_info.desegment_len = DESEGMENT_ONE_MORE_SEGMENT
        return
      end

    until length >= buffer:len()

  end


  -- initialization routine
  function example_protocol.init()
    print("in initialization of example protocol")

    local tcp_dissector_table = DissectorTable.get("tcp.port")

    -- TCP ports for protocol decoding
    for i,port in ipairs(example_tcp_ports) do
      print("register example protocol for port: " .. port) 
      tcp_dissector_table:add(port, example_protocol)
    end

  end

end
