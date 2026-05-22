local ArrayWindow= require 'ff6.vis.arraywindow'

local SRAMWindow = ArrayWindow:subclass()

function SRAMWindow:getCount() return 3 end

function SRAMWindow:showIndexUI()
end

return SRAMWindow
