$PBExportHeader$w_mant_eliminacion_caja.srw
forward
global type w_mant_eliminacion_caja from w_mant_tabla
end type
type dw_7 from datawindow within w_mant_eliminacion_caja
end type
end forward

global type w_mant_eliminacion_caja from w_mant_tabla
integer width = 2866
integer height = 1664
string title = "ELIMINACION DE CAJAS"
boolean maxbox = false
boolean resizable = false
event ue_validapassword ( )
dw_7 dw_7
end type
global w_mant_eliminacion_caja w_mant_eliminacion_caja

type variables
String				is_rut
uo_destinos		iuo_destinos
end variables

forward prototypes
public function boolean noexistecaja (long al_numero)
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel - Eliminación caja a caja"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean noexistecaja (long al_numero);Integer	li_Filaes,li_Cliente,li_Planta,li_especie,li_variedad, &
         li_predio,li_huerto,li_cuarte,li_etique,li_embdor,li_selecc, &
			li_pesado,li_estado,li_varrot,li_catego,li_cespak,li_nrlote,li_Respuesta, &
			li_enc_estado, li_enc_PCoPDA
Long		ll_numpal,ll_docrel,ll_productor,ll_numgia,ll_numcaja,ll_existepallet
Date		ld_fecha,ld_fecemb
String	ls_embala,ls_calibr,ls_cean14,ls_regcap

//li_Cliente	=	Integer(istr_mant.argumento[1])
//li_Planta	=	Integer(istr_mant.argumento[2])
//ll_numcaja	=	al_numero
//
//SELECT capr_docrel,capr_fecemb,prod_codigo,espe_codigo,vari_codigo,
//		prod_predio,prod_huerto,prod_cuarte,emba_codigo,etiq_codigo,
//		capr_calibr,capr_embala,capr_selecc,capr_pesado,
//		capr_cean14,capr_numpal,capr_regcap,capr_estado,capr_varrot,
//		capr_numgia,cate_codigo,capr_cespak,capr_nrlote
//		INTO	:ll_docrel,:ld_fecha,:ll_productor,:li_especie,:li_variedad,
//		:li_predio,:li_huerto,:li_cuarte,:ls_embala,:li_etique,
//		:ls_calibr,:li_embdor,:li_selecc,:li_pesado,
//		:ls_cean14,:ll_numpal,:ls_regcap,:li_estado,:li_varrot,
//		:ll_numgia,:li_catego,:li_cespak,:li_nrlote
//		FROM dba.spro_cajasprod
//		WHERE clie_codigo = :li_Cliente
//		AND   plde_codigo = :li_Planta
//		AND   capr_numero = :al_numero;
//
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca, "Lectura tabla spro_cajasprod")
//	RETURN True
//ELSEIF sqlca.SQLCode = 100 THEN
//		 MessageBox("Cuidado","Número de Caja No Existe en Tabla Respectiva!! ", &
//							StopSign!)	
//       RETURN True
//END IF
//
//SELECT Count(*) INTO :ll_existepallet
//	FROM dba.spro_cajasprodpallet
//	WHERE clie_codigo = :li_Cliente
//	AND   plde_codigo = :li_Planta
//	AND   capr_numero = :al_numero;
//
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca, "Lectura tabla spro_cajasprodpallet")
//	RETURN True
//ELSEIF IsNull(ll_existepallet) OR ll_existepallet = 0 THEN
//ELSE
//		MessageBox("Cuidado","Número de Caja Ya está Asignado a un Pallet!!", &
//		StopSign!)
//		RETURN True	
//END IF			
//
//dw_1.Object.clie_codigo[il_Fila]	=	li_Cliente
//dw_1.Object.plde_codigo[il_Fila]	=	li_Planta
//dw_1.Object.capr_numero[il_Fila]	=	al_numero
//dw_1.Object.capr_docrel[il_Fila]	=	ll_docrel
//dw_1.Object.capr_fecemb[il_Fila]	=	ld_fecha
//dw_1.Object.prod_codigo[il_Fila]	=	ll_productor
//dw_1.Object.espe_codigo[il_Fila]	=	li_especie
//dw_1.Object.vari_codigo[il_Fila]	=	li_variedad
//dw_1.Object.prod_predio[il_Fila]	=	li_predio
//dw_1.Object.prod_huerto[il_Fila]	=	li_huerto
//dw_1.Object.prod_cuarte[il_Fila]	=	li_cuarte
//dw_1.Object.emba_codigo[il_Fila]	=	ls_embala
//dw_1.Object.etiq_codigo[il_Fila]	=	li_etique
//dw_1.Object.capr_calibr[il_Fila]	=	ls_calibr
//dw_1.Object.capr_embala[il_Fila]	=	li_embdor
//dw_1.Object.capr_selecc[il_Fila]	=	li_selecc
//dw_1.Object.capr_pesado[il_Fila]	=	li_pesado
//dw_1.Object.capr_cean14[il_Fila]	=	ls_cean14
//dw_1.Object.capr_numpal[il_Fila]	=	ll_numpal
//dw_1.Object.capr_regcap[il_Fila]	=	ls_regcap
//dw_1.Object.capr_estado[il_Fila]	=	li_estado
//dw_1.Object.capr_varrot[il_Fila]	=	li_varrot
//dw_1.Object.capr_numgia[il_Fila]	=	ll_numgia
//dw_1.Object.cate_codigo[il_Fila]	=	li_catego
//dw_1.Object.capr_cespak[il_Fila]	=	li_cespak
//dw_1.Object.capr_nrlote[il_Fila]	=	li_nrlote
//
//pb_grabar.Enabled	=	True

RETURN False
end function

event open;Long ll_fila

x				= 	0
y				= 	0
This.Width		= 	dw_1.width + 540
im_menu		= 	m_principal

iuo_destinos	=	Create uo_destinos

This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

istr_mant.dw	= dw_1

buscar	= ""
ordenar	= ""

istr_mant.argumento[1]		=	String(gi_CodExport) 						//cliente
istr_mant.argumento[2]		=	String(gstr_paramplanta.codigoplanta)	//planta
istr_mant.argumento[3]		=	'0'											//caja
pb_grabar.Enabled			=	False

dw_1.SetTransObject(SQLCA)
dw_7.SetTransObject(SQLCA)
dw_1.SetRedraw(False)
dw_1.Reset()
dw_1.InsertRow(0)
dw_1.SetRedraw(True)
dw_1.SetFocus()

pb_eliminar.Enabled	= FALSE
pb_grabar.Enabled	= FALSE
pb_imprimir.Enabled	= FALSE
	

IF NOT IsNull(gstr_paramplanta.Password) AND Trim(gstr_paramplanta.Password) <> '' THEN
	PostEvent("ue_validapassword")
END IF

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

end event

on w_mant_eliminacion_caja.create
int iCurrent
call super::create
this.dw_7=create dw_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_7
end on

on w_mant_eliminacion_caja.destroy
call super::destroy
destroy(this.dw_7)
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

event ue_recuperadatos;w_main.SetMicroHelp("Recuperando Datos...")

Integer		li_filas

SetPointer(HourGlass!)
PostEvent("ue_listo")

li_filas	=	dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), Long(istr_mant.argumento[3]))

IF li_filas <> 1 THEN
	MessageBox("Error", "El correlativo ingresado no existe, ingrese otro")
	pb_eliminar.Enabled	= FALSE
	pb_grabar.Enabled	= FALSE
	pb_imprimir.Enabled	= FALSE
	dw_1.SetRedraw(FALSE)
	dw_1.Reset()
	dw_1.InsertRow(0)
	dw_1.SetRedraw(TRUE)
	dw_1.SetFocus()
ELSE
	pb_imprimir.Enabled	= TRUE
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled	= TRUE
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long 	ll_fila

dw_7.DataObject = "dw_info_spro_cajasprod_uvas"

IF iuo_destinos.Codigo = 3 THEN//England
//	dw_7.DataObject = "dw_info_spro_cajasprod_uvas_eng"
	dw_7.Modify("l_7.Visible = 1")
	dw_7.Modify("l_8.Visible = 1")
	dw_7.Modify("t_3.Visible = 0")
	dw_7.Modify("t_4.Visible = 0")
	dw_7.Modify("l_9.Visible = 0")
	dw_7.Modify("l_17.Visible = 0")
ELSEIF iuo_destinos.Codigo = 1 THEN//USA
//	dw_7.DataObject = "dw_info_spro_cajasprod_uvas_usa"
	dw_7.Modify("capr_calibr_1.Visible = 0")
	dw_7.Modify("capr_calibr_2.Visible = 0")
ELSEIF Integer(iuo_destinos.Codigo) < 1 THEN
	MessageBox("Atencion", "Utilizando formato Standar 'Otros Destinos'", Exclamation!)
END IF

dw_7.SetTransObject(SQLCA)

ll_Fila = dw_7.Retrieve(dw_1.Object.clie_codigo[1], &
					dw_1.Object.plde_codigo[1], &
					dw_1.Object.capr_numero[1], &
					dw_1.Object.capr_numero[1], &
					dw_1.Object.dest_codigo[1])

IF ll_fila > 0 THEN
	dw_7.Print()
END IF

SetPointer(Arrow!)
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_eliminacion_caja
integer x = 46
integer y = 36
integer width = 2295
integer height = 1464
integer taborder = 20
string dataobject = "dw_mant_spro_cajasprod_caja"
boolean vscrollbar = false
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

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula
Integer	li_Cliente, li_Planta
Long		ll_NroCaja, ll_NroPallet
String	ls_Data

SetNull(ls_Nula)

ls_Columna	=	dwo.Name
					
CHOOSE CASE ls_Columna
	CASE "caja_cajneo"
		istr_mant.argumento[3]	=	data
		Parent.TriggerEvent('ue_recuperadatos')
		
	CASE "destino"
		IF NOT iuo_destinos.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
END CHOOSE


end event

event dw_1::itemerror;RETURN 1
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_eliminacion_caja
boolean visible = false
integer x = 183
integer y = 176
integer width = 2359
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_eliminacion_caja
boolean visible = false
integer x = 3643
integer y = 128
integer taborder = 0
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_eliminacion_caja
boolean visible = false
integer x = 3639
integer y = 424
integer taborder = 0
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_eliminacion_caja
boolean visible = false
integer x = 3639
integer y = 604
integer taborder = 0
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_eliminacion_caja
integer y = 876
integer taborder = 0
string picturename = "\desarrollo\bmp\deletee.bmp"
string disabledname = "\desarrollo\bmp\deleted.bmp"
end type

event pb_eliminar::clicked;call super::clicked;SetPointer(HourGlass!)

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

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_eliminacion_caja
integer y = 732
integer taborder = 30
string picturename = "\Desarrollo\Bmp\ACEPTAE.BMP"
string disabledname = "\Desarrollo\Bmp\ACEPTAD.BMP"
end type

event pb_grabar::clicked;pb_salir.TriggerEvent("clicked")
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_eliminacion_caja
integer y = 1028
integer taborder = 0
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_eliminacion_caja
integer y = 1300
integer taborder = 40
end type

type dw_7 from datawindow within w_mant_eliminacion_caja
boolean visible = false
integer x = 155
integer y = 60
integer width = 1138
integer height = 848
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_uvas"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

