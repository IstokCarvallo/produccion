$PBExportHeader$w_mant_consulta_existencia_granel.srw
forward
global type w_mant_consulta_existencia_granel from w_mant_tabla
end type
type dw_2 from datawindow within w_mant_consulta_existencia_granel
end type
type dw_3 from datawindow within w_mant_consulta_existencia_granel
end type
end forward

global type w_mant_consulta_existencia_granel from w_mant_tabla
integer width = 3282
integer height = 1668
string title = "GUIAS FALTANTES EN EXISTENCIA"
boolean maxbox = false
boolean resizable = false
event ue_validapassword ( )
dw_2 dw_2
dw_3 dw_3
end type
global w_mant_consulta_existencia_granel w_mant_consulta_existencia_granel

type variables
String	is_rut
Boolean	ib_ConectadoExistencia

Transaction			sqlexi
end variables

forward prototypes
public function boolean conexionexistencia ()
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Producción"
istr_mant.Argumento[2]	=	gs_Password

OpenWithParm(w_password_guia, istr_mant)

istr_mant	=	Message.PowerObjectParm


end event

public function boolean conexionexistencia ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ib_ConectadoExistencia THEN
	DISCONNECT USING sqlexi;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT cone_nomodb,cone_nomser,cone_nombas,
		cone_nodbms,cone_nomusu,cone_passwo  
 INTO :ls_nomodb,:ls_nomser,:ls_nombas,
		:ls_nodbms,:ls_Usuario,:ls_Password
 FROM dba.prodconectividad   
WHERE cone_codigo = 96;

sqlexi.ServerName	=	ls_nomser
sqlexi.DataBase	=	ls_nombas
sqlexi.Dbms			= 	ls_nodbms
sqlexi.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
CONNECT USING sqlexi;

IF sqlexi.SQLCode = 0 THEN
	ib_ConectadoExistencia	=	True
ELSE
	ib_ConectadoExistencia	=	False
END IF

RETURN ib_ConectadoExistencia

end function

on w_mant_consulta_existencia_granel.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
end on

on w_mant_consulta_existencia_granel.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
end on

event ue_validaborrar;call super::ue_validaborrar;IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
	Message.DoubleParm = 1
ELSE
	Message.DoubleParm = -1
END IF

RETURN
end event

event closequery;//
end event

event open;call super::open;Long	ll_fila

dw_2.SetTransObject(sqlca)

ll_fila = dw_2.Retrieve()

IF ll_fila > 0 THEN TriggerEvent("ue_validapassword")

IF istr_mant.Respuesta = 0 THEN
	Close(w_mant_consulta_existencia_granel)
	Return 1
END IF	


w_mant_consulta_existencia_granel.TriggerEvent("ue_recuperadatos")
end event

event ue_recuperadatos;Long		ll_fila, ll_guia, ll_new, ll_numero, ll_nueva
Integer	li_cliente, li_coneccion, li_coneccion2

FOR ll_fila = 1 TO dw_2.RowCount()
	li_Cliente	=	dw_2.Object.clie_codigo[ll_fila]
		
	SELECT 	cone_codigo
	INTO   	:li_coneccion
	FROM dba.clientesprod
	WHERE clie_codigo = :li_Cliente;
	
	IF li_coneccion <> li_coneccion2 THEN
		sqlexi	=	CREATE Transaction
		IF Conexionexistencia() THEN
			li_coneccion2 = li_coneccion
			dw_3.SetTransObject(sqlexi)
			ll_guia 	= dw_2.Object.lote_guisii[ll_fila]
			ll_numero = dw_2.Object.mfge_numero[ll_fila]
			dw_3.Retrieve(ll_guia,ll_numero)
			
	//		IF dw_3.RowCount() = 0 THEN
				
				
//				dw_1.Object.bodega[ll_new] 	  = dw_2.Object.bode_codigo[ll_fila]
//				dw_1.Object.existencia[ll_new]  = dw_2.Object.mden_numero[ll_fila]
	//		END IF
			IF dw_3.RowCount() > 0 THEN
				FOR ll_nueva = 1 TO dw_3.RowCount()
					IF NOT isnull(dw_3.Object.bode_codigo[1]) OR dw_3.Object.bode_codigo[1] <> 0 THEN
						ll_new	=	dw_1.InsertRow(0)
						dw_1.Object.mfge_guisii[ll_new] = dw_2.Object.lote_guisii[ll_fila]
						dw_1.Object.mfge_numero[ll_new] = dw_2.Object.mfge_numero[ll_fila]
						dw_1.Object.bodega[ll_new] 	  = dw_3.Object.bode_codigo[ll_nueva]
						dw_1.Object.existencia[ll_new]  = dw_3.Object.mden_numero[ll_nueva]
						dw_1.Object.mfge_fecmov[ll_new] = dw_3.Object.mden_fecmov[ll_nueva]
						dw_1.Object.defg_tipdoc[ll_new] = dw_3.Object.tpmv_tipomv[ll_nueva]
					END IF	
				NEXT	
			END IF	
		ELSE	
			MessageBox("Atención Guía "+String(ll_guia), "No se Puede Conectar con Base de Existencia.",Exclamation!, OK!)
		END IF	
		
		IF ll_fila + 1 <=  dw_2.RowCount() THEN
			li_Cliente	=	dw_2.Object.clie_codigo[ll_fila + 1]
		
			SELECT 	cone_codigo
			INTO   	:li_coneccion
			FROM dba.clientesprod
			WHERE clie_codigo = :li_Cliente;
		
			IF li_coneccion2 <> li_coneccion THEN
				DISCONNECT USING sqlexi;
			END IF	
			
		END IF	
	ELSE	
		ll_guia 		= dw_2.Object.lote_guisii[ll_fila]
		ll_numero 	= dw_2.Object.mfge_numero[ll_fila]
		dw_3.Retrieve(ll_guia,ll_numero)
		
//		IF dw_3.RowCount() = 0 THEN
		
		
		IF dw_3.RowCount() > 0 THEN
			FOR ll_nueva = 1 TO dw_3.RowCount()
				IF NOT isnull(dw_3.Object.bode_codigo[1]) OR dw_3.Object.bode_codigo[1] <> 0 THEN
					ll_new	=	dw_1.InsertRow(0)
					dw_1.Object.mfge_guisii[ll_new] = dw_2.Object.lote_guisii[ll_fila]
					dw_1.Object.mfge_numero[ll_new] = dw_2.Object.mfge_numero[ll_fila]
					dw_1.Object.bodega[ll_new] 	  = dw_3.Object.bode_codigo[ll_nueva]
					dw_1.Object.existencia[ll_new]  = dw_3.Object.mden_numero[ll_nueva]
					dw_1.Object.mfge_fecmov[ll_new] = dw_3.Object.mden_fecmov[ll_nueva]
					dw_1.Object.defg_tipdoc[ll_new] = dw_3.Object.tpmv_tipomv[ll_nueva]
				END IF	
			NEXT	
		END IF	
//		END IF
		
		IF ll_fila + 1 <=  dw_2.RowCount() THEN
			li_Cliente	=	dw_2.Object.clie_codigo[ll_fila + 1]
		
			SELECT 	cone_codigo
			INTO   	:li_coneccion
			FROM dba.clientesprod
			WHERE clie_codigo = :li_Cliente;
		
			IF li_coneccion2 <> li_coneccion THEN
				DISCONNECT USING sqlexi;
			END IF	
			li_coneccion2 = li_coneccion
		END IF	
	END IF
NEXT		
DISCONNECT USING sqlexi;
end event

event close;//
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_consulta_existencia_granel
integer x = 46
integer y = 36
integer width = 2853
integer height = 1504
integer taborder = 20
string dataobject = "dw_mues_llena_con_guias"
boolean hscrollbar = true
boolean livescroll = false
end type

event dw_1::clicked;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::getfocus;//
end event

event dw_1::doubleclicked;//
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	IF dwo.Name = "empr_rutemp" THEN
		IF is_rut <> "" THEN
			This.SetItem(1, "empr_rutemp", String(Double(Mid(is_rut, 1, 9)), "#########") + Mid(is_rut, 10))
		END IF
	ELSE
		This.SetItem(1, "empr_rutemp", is_rut)
	END IF
END IF
end event

event dw_1::itemerror;RETURN 1
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 183
integer y = 176
integer width = 2359
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 3643
integer y = 128
integer taborder = 0
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 3639
integer taborder = 0
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 3639
integer y = 604
integer taborder = 0
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 3639
integer y = 784
integer taborder = 0
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 3008
integer y = 372
integer taborder = 30
string picturename = "\Desarrollo 12\Imagenes\Botones\EliminaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\EliminarDisab.png"
end type

event pb_grabar::clicked;SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN 
	This.TriggerEvent("ue_nuevo")
	//RETURN
ELSE	
	dw_1.ResetUpdate()							
	IF dw_1.DeleteRow(0) = 1 THEN
		IF dw_1.Update() = 1 THEN
			Commit;
			IF SQLCA.SQLCode <> 0 THEN
				RollBack;
				MessageBox("Eliminación de Caja", "El Proceso no se pudo ejecutar.~r~n"+&
								"Codigo de Error : "+String(SQLCA.SQLdbCode)+"~r~n"+&
								"Mensaje         : "+SQLCA.SQLErrText)
			ELSE
				MessageBox("Atención","Caja Eliminada.", Exclamation!,Ok!)
				dw_1.SetRedraw(False)
				dw_1.Reset()
				dw_1.InsertRow(0)
				dw_1.SetRedraw(True)
				dw_1.SetFocus()
				
				//dw_1.InsertRow(0)
				pb_grabar.Enabled	=	False
			END IF
		ELSE
			MessageBox("Eliminación de Caja", "El Proceso no se pudo ejecutar.~r~n"+&
								"Codigo de Error : "+String(SQLCA.SQLdbCode)+"~r~n"+&
								"Mensaje         : "+SQLCA.SQLErrText)
			dw_1.Reset()
		END IF
	END IF	
END IF
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 3639
integer y = 1144
integer taborder = 0
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_consulta_existencia_granel
integer x = 3017
integer y = 1232
integer taborder = 40
end type

type dw_2 from datawindow within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 1495
integer y = 1456
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_lote_recepcion_granel"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_mant_consulta_existencia_granel
boolean visible = false
integer x = 1979
integer y = 1476
integer width = 686
integer height = 400
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_guia_existencia"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

