# *************************************************************************
#     Copyright (C) 2010  Piotr Karasinski                                 
#                                                                         
#     This program is free software: you can redistribute it and/or modify 
#     it under the terms of the GNU General Public License as published by 
#     the Free Software Foundation, either version 3 of the License, or    
#     (at your option) any later version.                                  
#                                                                         
#     This program is distributed in the hope that it will be useful,      
#     but WITHOUT ANY WARRANTY; without even the implied warranty of       
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        
#     GNU General Public License for more details.                         
#                                                                         
#     You should have received a copy of the GNU General Public License    
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#                                                                         
#     email: caleb9@users.sourceforge.net                                    
# *************************************************************************
#
# If you have to customize make process, please try to see and edit
# Makefile.include file in ToneGrid's main directory first.

include ../Makefile.include

PRJ_NAME=tonegridproject
PRJ_PATH=.
LPR_FILE=$(PRJ_PATH)/$(PRJ_NAME).lpr
OUT_FILE=$(PRJ_PATH)/$(PRJ_NAME)

all: project

project:
	fpc $(LPR_FILE) \
	-MObjFPC -Scgi -O1 -gl -WG -vewnhi -l \
	-Fu../package/lib/$(ARCH_OS)/ \
	-Fu$(LAZ_DIR)/lcl/units/$(ARCH_OS)/ \
	-Fu$(LAZ_DIR)/lcl/units/$(ARCH_OS)/$(WIDG)/ \
	-Fu$(LAZ_DIR)/packager/units/$(ARCH_OS)/ \
	-Fu$(PRJ_PATH) -o$(OUT_FILE) -dLCL -dLCL$(WIDG)

clean:
	rm -fv $(OUT_FILE) *.{bak,ppu,ppw,ppl,o,a} *~
