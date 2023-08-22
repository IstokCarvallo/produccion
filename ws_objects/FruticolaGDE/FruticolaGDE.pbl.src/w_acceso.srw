$PBExportHeader$w_acceso.srw
forward
global type w_acceso from w_acceso_usuario
end type
end forward

global type w_acceso from w_acceso_usuario
end type
global w_acceso w_acceso

event ue_conectar;call super::ue_conectar;SetPointer(HourGlass!)

st_conect.text = "Abriendo Base de Datos..."

String	ls_server1 = "12345678", ls_nombre, ls_Dbms
Integer	li_posic, li_resp

sqlca.SQLCode	=	1
ii_pos				=	1
ls_server1		=	ProfileString(gstr_apl.ini, is_base, "servername", "12345678")
ls_Dbms			=	ProFileString(gstr_apl.ini, is_base, "dbms", "ODBC")

IF ls_server1 <> "12345678" THEN
	ls_nombre			=	ProfileString(gstr_apl.ini, is_base, "NombreOdbc", "")
	sqlca.Dbms			=	ProFileString(gstr_apl.ini, is_base, "dbms", "ODBC")
	sqlca.ServerName	=	ProfileString(gstr_apl.ini, is_base, "servername", "")
	sqlca.DataBase		=	ProFileString(gstr_apl.ini, is_base, "database", "")
	
	IF ls_Dbms = "ODBC" THEN
		sqlca.DbParm	=	"ConnectString='DSN=" + ls_nombre + &
								";UID=" + sle_nombre.text + &
								";PWD=" + sle_clave.text + "'DisableBind=1," + &
								"ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'" + &
								"// ;PBUseProcOwner = " + '"Yes"'
								
	ELSE
		sqlca.LogId			=	sle_nombre.text
		sqlca.LogPass		=	sle_clave.text
		sqlca.Autocommit	=	True
	END IF
ELSE
	MessageBox(This.Title,"No se puede ejecutar la aplicación por la falta de archivo " &
					+ gstr_apl.ini + ". Verifique que el archivo exista y que esté en el directorio " + &
					"donde la aplicación esté corriendo o en el PATH del Computador del cliente.",StopSign!)
	Halt Close
	Return
END IF

IF Len(Trim(sle_nombre.text)) <> 0 THEN
	sle_clave.SetFocus()
END IF

SetPointer(HourGlass!)

DO
	CONNECT Using SQLCA ; 

	IF sqlca.SQLCode <> 0 THEN
		IF sqlca.SQLDBCode = -103 THEN
			IF MessageBox("Error de Conexión", "Usuario o Password ingresado está incorrecto.", &
								Information!, RetryCancel!) = 1 THEN
				sle_clave.SetFocus()
			END IF
			
			RETURN
		ELSEIF sqlca.SQLDBCode = -102 THEN
			MessageBox("Error de Conexión", "Demasiados Usuarios conectados a la Base.~r" + &
							"Consulte con Administrador", StopSign!, Ok!)
				RETURN
		ELSEIF sqlca.SQLDBCode <> 0 THEN
			st_conect.text = "Error de Conexión..."
			F_ErrorBaseDatos(sqlca, This.Title)
			RETURN
		END IF
	END IF
	
	sle_nombre.text	= 	sqlca.UserId
LOOP UNTIL sqlca.SQLCode <> 0

st_conect.text	= "Usuario Conectado..."
ib_connected	= True

This.TriggerEvent("ue_datosempresa")

SetPointer(Arrow!)
end event

event ue_datosempresa;call super::ue_datosempresa;String	empr_razsoc, empr_nombre, empr_rutemp, empr_direcc, empr_comuna, &
			empr_ciudad, empr_giroem, empr_nrotel, empr_nrofax, empr_repleg, &
			empr_rutrle, empr_oficin, empr_dirres
Integer  empr_admenv			

SELECT	empr_razsoc, empr_nombre, empr_rutemp, empr_direcc, empr_comuna,
			empr_ciudad, empr_giroem, empr_nrotel, empr_nrofax, empr_repleg,
			empr_rutrle, empr_oficin, empr_dirres, expo_codigo, empr_especi,
			empr_admenv, empr_passwd	
	INTO	:empr_razsoc, :empr_nombre, :empr_rutemp, :empr_direcc, :empr_comuna,
			:empr_ciudad, :empr_giroem, :empr_nrotel, :empr_nrofax, :empr_repleg,
			:empr_rutrle, :empr_oficin, :empr_dirres, :gi_codexport, :gi_CodEspecie,
			:empr_admenv, :gs_Password
	FROM	dba.parempresa;
	
gstr_apl.razon_social	=	empr_razsoc
gstr_apl.nom_empresa		=	empr_nombre
gstr_apl.rut_empresa		=	empr_rutemp
gstr_apl.dir_empresa		=	empr_direcc
gstr_apl.com_empresa		=	empr_comuna
gstr_apl.ciu_empresa		=	empr_ciudad
gstr_apl.gir_empresa		=	empr_giroem
gstr_apl.tel_empresa		=	empr_nrotel
gstr_apl.fax_empresa		=	empr_nrofax
gstr_apl.rep_legal		=	empr_repleg
gstr_apl.rut_replegal	=	empr_rutrle
gstr_apl.Oficina			=	empr_oficin
gstr_apl.DirRespaldo		=	empr_dirres
gi_admenvase				=	empr_admenv


//gstr_apl.referencia	=	ProfileString(gstr_apl.ini, is_base, "Temporada", "")
gs_Base					=	is_base


end event

on w_acceso.create
call super::create
end on

on w_acceso.destroy
call super::destroy
end on

type p_aceptar from w_acceso_usuario`p_aceptar within w_acceso
end type

type p_cerrar from w_acceso_usuario`p_cerrar within w_acceso
end type

type sle_nombre from w_acceso_usuario`sle_nombre within w_acceso
end type

type p_1 from w_acceso_usuario`p_1 within w_acceso
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type ddlb_bases from w_acceso_usuario`ddlb_bases within w_acceso
end type

type st_titulo from w_acceso_usuario`st_titulo within w_acceso
end type

type st_empresa from w_acceso_usuario`st_empresa within w_acceso
end type

type st_conect from w_acceso_usuario`st_conect within w_acceso
end type

type sle_clave from w_acceso_usuario`sle_clave within w_acceso
end type

type st_2 from w_acceso_usuario`st_2 within w_acceso
end type

type st_1 from w_acceso_usuario`st_1 within w_acceso
end type

type p_mono from w_acceso_usuario`p_mono within w_acceso
boolean map3dcolors = false
end type

