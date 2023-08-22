$PBExportHeader$w_mant_eliminacion_caja.srw
forward
global type w_mant_eliminacion_caja from w_mant_tabla
end type
end forward

global type w_mant_eliminacion_caja from w_mant_tabla
integer width = 2789
integer height = 1668
string title = "ELIMINACION DE CAJA"
boolean maxbox = false
boolean resizable = false
event ue_validapassword ( )
end type
global w_mant_eliminacion_caja w_mant_eliminacion_caja

type variables
String	is_rut
end variables

forward prototypes
public function boolean noexistecaja (long al_numero)
end prototypes

event ue_validapassword();Str_mant			lstr_mant

lstr_mant.Argumento[1]	=	"Producción"
lstr_mant.Argumento[2]	=	gs_Password

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

li_Cliente	=	Integer(istr_mant.argumento[1])
li_Planta	=	Integer(istr_mant.argumento[2])
ll_numcaja	=	al_numero

SELECT capr_docrel,capr_fecemb,prod_codigo,espe_codigo,vari_codigo,
		prod_predio,prod_huerto,prod_cuarte,emba_codigo,etiq_codigo,
		capr_calibr,capr_embala,capr_selecc,capr_pesado,
		capr_cean14,capr_numpal,capr_regcap,capr_estado,capr_varrot,
		capr_numgia,cate_codigo,capr_cespak,capr_nrlote
		INTO	:ll_docrel,:ld_fecha,:ll_productor,:li_especie,:li_variedad,
		:li_predio,:li_huerto,:li_cuarte,:ls_embala,:li_etique,
		:ls_calibr,:li_embdor,:li_selecc,:li_pesado,
		:ls_cean14,:ll_numpal,:ls_regcap,:li_estado,:li_varrot,
		:ll_numgia,:li_catego,:li_cespak,:li_nrlote
		FROM dba.spro_cajasprod
		WHERE clie_codigo = :li_Cliente
		AND   plde_codigo = :li_Planta
		AND   capr_numero = :al_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla spro_cajasprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
		 MessageBox("Cuidado","Número de Caja No Existe en Tabla Respectiva!! ", &
							StopSign!)	
       RETURN True
END IF

SELECT Count(*) INTO :ll_existepallet
	FROM dba.spro_cajasprodpallet
	WHERE clie_codigo = :li_Cliente
	AND   plde_codigo = :li_Planta
	AND   capr_numero = :al_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla spro_cajasprodpallet")
	RETURN True
ELSEIF IsNull(ll_existepallet) OR ll_existepallet = 0 THEN
ELSE
		MessageBox("Cuidado","Número de Caja Ya está Asignado a un Pallet!!", &
		StopSign!)
		RETURN True	
END IF			

dw_1.Object.clie_codigo[il_Fila]	=	li_Cliente
dw_1.Object.plde_codigo[il_Fila]	=	li_Planta
dw_1.Object.capr_numero[il_Fila]	=	al_numero
dw_1.Object.capr_docrel[il_Fila]	=	ll_docrel
dw_1.Object.capr_fecemb[il_Fila]	=	ld_fecha
dw_1.Object.prod_codigo[il_Fila]	=	ll_productor
dw_1.Object.espe_codigo[il_Fila]	=	li_especie
dw_1.Object.vari_codigo[il_Fila]	=	li_variedad
dw_1.Object.prod_predio[il_Fila]	=	li_predio
dw_1.Object.prod_huerto[il_Fila]	=	li_huerto
dw_1.Object.prod_cuarte[il_Fila]	=	li_cuarte
dw_1.Object.emba_codigo[il_Fila]	=	ls_embala
dw_1.Object.etiq_codigo[il_Fila]	=	li_etique
dw_1.Object.capr_calibr[il_Fila]	=	ls_calibr
dw_1.Object.capr_embala[il_Fila]	=	li_embdor
dw_1.Object.capr_selecc[il_Fila]	=	li_selecc
dw_1.Object.capr_pesado[il_Fila]	=	li_pesado
dw_1.Object.capr_cean14[il_Fila]	=	ls_cean14
dw_1.Object.capr_numpal[il_Fila]	=	ll_numpal
dw_1.Object.capr_regcap[il_Fila]	=	ls_regcap
dw_1.Object.capr_estado[il_Fila]	=	li_estado
dw_1.Object.capr_varrot[il_Fila]	=	li_varrot
dw_1.Object.capr_numgia[il_Fila]	=	ll_numgia
dw_1.Object.cate_codigo[il_Fila]	=	li_catego
dw_1.Object.capr_cespak[il_Fila]	=	li_cespak
dw_1.Object.capr_nrlote[il_Fila]	=	li_nrlote

pb_grabar.Enabled	=	True

RETURN False
end function

event open;Long ll_fila

x				= 0
y				= 0
This.Width	= dw_1.width + 540
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

istr_mant.dw	= dw_1

buscar	= ""
ordenar	= ""

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	String(gi_CodPlanta)
pb_grabar.Enabled			=	False

dw_1.SetRedraw(False)
dw_1.Reset()
dw_1.InsertRow(0)
dw_1.SetRedraw(True)
dw_1.SetFocus()

//ll_fila = dw_1.Retrieve()
//IF ll_fila = 0 THEN
//	dw_1.InsertRow(0)
//END IF	
//IF ll_fila > 0 THEN PostEvent("ue_validapassword")

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

end event

on w_mant_eliminacion_caja.create
call super::create
end on

on w_mant_eliminacion_caja.destroy
call super::destroy
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

type dw_1 from w_mant_tabla`dw_1 within w_mant_eliminacion_caja
integer x = 46
integer y = 36
integer width = 2304
integer height = 1504
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
		IF Len(Data) < 16 THEN
			MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", &
							StopSign!)
			This. SetItem(il_Fila,ls_Columna,ls_Nula)
			RETURN 1
		END IF
		
		ls_Data		=	Data
		
		li_Planta	=	Integer(Mid(Data, 3, 4))
		ll_NroCaja	=	Long(Mid(Data, 7, 10))		
		
		li_Cliente	=	Integer(istr_mant.Argumento[1])
		//li_Planta	=	Integer(istr_mant.Argumento[2])

   	IF NoExisteCaja(ll_NroCaja) THEN							
			This.SetItem(1, 'caja_cajneo', ls_Nula)
			RETURN 1
		END IF	

		dw_1.Object.caja_cajneo[1]	=	ls_Data
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
boolean visible = false
integer x = 3639
integer y = 784
integer taborder = 0
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_eliminacion_caja
integer y = 836
integer taborder = 30
string picturename = "\desarrollo\bmp\deletee.bmp"
string disabledname = "\desarrollo\bmp\deleted.bmp"
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

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_eliminacion_caja
boolean visible = false
integer x = 3639
integer y = 1144
integer taborder = 0
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_eliminacion_caja
integer y = 1300
integer taborder = 40
end type

