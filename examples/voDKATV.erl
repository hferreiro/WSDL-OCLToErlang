-module(voDKATV).

-compile(export_all).

-include("vodkatv.hrl").

init() ->
    inets:start(),
    case vodkatv_sut:find_all_rooms() of
        {ok, R} ->
            Rooms = get_value_or_empty_list(R#rooms.room),
            lists:map(
                fun(Room) ->
                    vodkatv_sut:delete_room([Room#roomType.roomId])
                end, Rooms);
        Error ->
            throw(Error)
    end.

get_state_rooms()->
    {ok, R} = vodkatv_sut:find_all_rooms(),
    case R#rooms.errors of
        undefined ->
            Rooms = get_value_or_empty_list(R#rooms.room),
            sets:from_list(lists:map(fun(Room) ->
                Room#roomType.roomId
            end, Rooms));
        _Errors ->
            sets:new()
    end.

get_state_devices()->
    {ok, R} = vodkatv_sut:find_devices(1, 10000, undefined, undefined, undefined),
    case R#devices.errors of
        undefined ->
            Devices = get_value_or_empty_list(R#devices.device),
            sets:from_list(lists:map(fun(Device) ->
                to_device_type(Device)
            end, Devices));
        _Errors ->
            sets:new()
    end.

createRoom(RoomId, Description, Tag) ->
    {ok, R} = vodkatv_sut:create_room(RoomId, Description, Tag),
    case R#room.errors of
        undefined ->
            to_room(R);
        Errors ->
            to_errors(Errors)
    end.

findRoomById(RoomId) ->
    {ok, R} = vodkatv_sut:find_room_by_id(RoomId),
    case R#room.errors of
        undefined ->
            to_room(R);
        Errors ->
            to_errors(Errors)
    end.

findAllRooms() ->
    {ok, R} = vodkatv_sut:find_all_rooms(),
    case R#rooms.errors of
        undefined ->
            Rooms = get_value_or_empty_list(R#rooms.room),
            RoomsAttrs = lists:map(fun(Room) ->
                to_room_type(Room)
            end, Rooms),
            {room, RoomsAttrs};
        Errors ->
            to_errors(Errors)
    end.

deleteRoom(RoomId) ->
    {ok, R} = vodkatv_sut:delete_room([RoomId]),
    case R#rooms.errors of
        undefined ->
            RoomsAttrs = lists:map(fun(Room) ->
                to_room_type(Room)
            end, R#rooms.room),
            {room, RoomsAttrs};
        Errors ->
            to_errors(Errors)
    end.

createDevice(PhysicalId, DeviceClass, RoomId, Description, Tag)->
    {ok, R} = vodkatv_sut:create_device(PhysicalId, DeviceClass, RoomId, Description, Tag),
    case R#device.errors of
        undefined ->
            to_device(R);
        Errors ->
            to_errors(Errors)
    end.

findDevicesByRoom(RoomId) ->
    {ok, R} = vodkatv_sut:find_devices_by_room(RoomId),
    case R#devices.errors of
        undefined ->
            Devices = get_value_or_empty_list(R#devices.device),
            DevicesAttrs = lists:map(fun(Device) ->
                to_device_type(Device)
            end, Devices),
            {device, DevicesAttrs};
        Errors ->
            to_errors(Errors)
    end.

findDeviceById(DeviceId) ->
    {ok, R} = vodkatv_sut:find_device_by_id(DeviceId),
    case R#device.errors of
        undefined ->
            to_device(R);
        Errors ->
            to_errors(Errors)
    end.

findDevices(StartIndex, Count, SortBy, Order, Query)->
    {ok, R} = vodkatv_sut:find_devices(StartIndex, Count, SortBy, Order, Query),
    case R#devices.errors of
        undefined ->
            Devices = get_value_or_empty_list(R#devices.device),
            DevicesAttrs = lists:map(fun(Device) ->
                to_device_type(Device)
            end, Devices),
            {device, DevicesAttrs};
        Errors ->
            to_errors(Errors)
    end.

updateDevice(DeviceId, PhysicalId, DeviceClass, RoomId, Description, Tag)->
    {ok, R} = vodkatv_sut:update_device(DeviceId, PhysicalId, DeviceClass,
            RoomId, Description, Tag),
    case R#device.errors of
        undefined ->
            to_device(R);
        Errors ->
            to_errors(Errors)
    end.

deleteDevice(DeviceId) ->
    {ok, R} = vodkatv_sut:delete_device([DeviceId]),
    case R#devices.errors of
        undefined ->
            DevicesAttrs = lists:map(fun(Device) ->
                to_device_type(Device)
            end, R#devices.device),
            {device, DevicesAttrs};
        Errors ->
            to_errors(Errors)
    end.

to_room_type(Room) ->
    {roomType, [
        {roomId, Room#roomType.roomId},
        {description, Room#roomType.description},
        {tag, Room#roomType.tag}
    ]}.

to_room(Room) ->
    {room, [
        {roomId, Room#room.roomId},
        {description, Room#room.description},
        {tag, Room#room.tag}
    ]}.

to_device_type(Device) ->
    {deviceType, [
        {id, Device#deviceType.id},
        {physicalId, Device#deviceType.physicalId},
        {deviceClass, Device#deviceType.deviceClass},
        {roomId, Device#deviceType.roomId},
        {description, Device#deviceType.description},
        {tag, Device#deviceType.tag},
        {version, Device#deviceType.version},
        {lastUpdateTime, Device#deviceType.lastUpdateTime}
    ]}.

to_device(Device) ->
    {device, [
        {id, Device#device.id},
        {physicalId, Device#device.physicalId},
        {deviceClass, Device#device.deviceClass},
        {roomId, Device#device.roomId},
        {description, Device#device.description},
        {tag, Device#device.tag},
        {version, Device#device.version},
        {lastUpdateTime, Device#device.lastUpdateTime}
    ]}.

to_errors(Errors) ->
    ErrorsAttr = lists:map(fun(Error) ->
        ErrorParams = Error#error.params,
        ErrorParamsAttr = lists:map(fun(ErrorParam) ->
            {errorParams, [
                {name, ErrorParam#errorParam.name},
                {value, ErrorParam#errorParam.value}
            ]}
        end, ErrorParams#errorParams.param),
        {error, [{code, Error#error.code}, {params, ErrorParamsAttr}]}
    end, Errors#errors.error),
    {errors, ErrorsAttr}.

get_value_or_empty_list(undefined) ->
    [];
get_value_or_empty_list(Value) ->
    Value.
