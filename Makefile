PRJ_PATH=./project
PKG_PATH=./package

all:
	make -C $(PKG_PATH)
	make -C $(PRJ_PATH)

clean:
	make -C $(PKG_PATH) clean
	make -C $(PRJ_PATH) clean
