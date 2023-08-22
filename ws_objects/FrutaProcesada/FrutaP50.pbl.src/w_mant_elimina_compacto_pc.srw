$PBExportHeader$w_mant_elimina_compacto_pc.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_elimina_compacto_pc from w_mant_directo
end type
type st_2 from statictext within w_mant_elimina_compacto_pc
end type
type st_1 from statictext within w_mant_elimina_compacto_pc
end type
type em_numero from editmask within w_mant_elimina_compacto_pc
end type
type st_3 from statictext within w_mant_elimina_compacto_pc
end type
type dw_3 from datawindow within w_mant_elimina_compacto_pc
end type
type dw_4 from datawindow within w_mant_elimina_compacto_pc
end type
type dw_2 from datawindow within w_mant_elimina_compacto_pc
end type
type cb_1 from commandbutton within w_mant_elimina_compacto_pc
end type
type cb_2 from commandbutton within w_mant_elimina_compacto_pc
end type
end forward

global type w_mant_elimina_compacto_pc from w_mant_directo
integer x = 155
integer y = 156
integer width = 3406
integer height = 1880
string title = "ELIMINA COMPACTO DE CAJA"
event ue_validaborrar ( )
st_2 st_2
st_1 st_1
em_numero em_numero
st_3 st_3
dw_3 dw_3
dw_4 dw_4
dw_2 dw_2
cb_1 cb_1
cb_2 cb_2
end type
global w_mant_elimina_compacto_pc w_mant_elimina_compacto_pc

type variables
DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta
Integer ii_pallet, ii_nuevo

//str_busqueda istr_busq
//str_mant istr_mant
Integer	ii_tipo, ii_contador
String	is_report

uo_plantadesp			iuo_Planta
uo_clientesprod			iuo_ClienteProd



end variables

forward prototypes
public function boolean existepallet (long al_numero)
public subroutine wf_selecciona (string as_tecla, long al_fila)
protected function boolean wf_actualiza_db ()
end prototypes

public function boolean existepallet (long al_numero);Integer	li_codexp, li_planta, li_Tipova
Date		ld_fecha
String	ls_Embarque
Long		ll_pallet

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF al_numero <> 0 OR li_planta = 0 THEN

		SELECT pe.paen_numero
		INTO	:ll_pallet
		FROM	dba.palletencab as pe
		WHERE	pe.plde_codigo =	:li_planta
		AND	pe.clie_codigo	=	:li_codexp
		AND	pe.paen_numero	=	:al_numero
		AND	pe.paen_Estado =	1
		AND   not exists(select *
		From dba.spro_palletencab as sp
		Where sp.clie_codigo = pe.clie_codigo
		And   sp.plde_codigo = pe.plde_codigo
		And   sp.paen_numero = pe.paen_numero);
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
		em_numero.Text = ''
		em_numero.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Número de Pallet Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.Text = ''
		em_numero.SetFocus()
		RETURN False
	END IF
	RETURN TRUE
END IF	
	
end function

public subroutine wf_selecciona (string as_tecla, long al_fila);/*
Función que Administra la Selección de Filas
Argumentos :	as_tecla		Tecla que acompaña el Click
					al_fila		Fila donde se hizo el Click
*/
Long		ll_Filas, ll_FilaIni, ll_FilaSel
Integer	li_Increm

dw_2.SetRedraw(False)

CHOOSE CASE as_tecla
	CASE "Shift"
		ll_filaini	=	dw_2.GetSelectedRow(0)
		
		IF ll_filaini = 0 THEN
			dw_2.SelectRow(al_fila, Not dw_2.IsSelected(al_fila))
		ELSE
			li_increm	=	Sign(al_fila - ll_filaini)
			
			IF li_Increm < 0 THEN
				ll_filaini 	+=	li_increm
			
				DO WHILE ll_filaini <> al_fila + li_increm
					dw_2.SelectRow(ll_filaini, Not dw_2.IsSelected(al_fila))
					ll_filaini 	+=	li_increm
				LOOP
			ELSE
				DO WHILE ll_FilaIni > 0
					ll_FilaSel	=	ll_FilaIni
					ll_FilaIni	=	dw_2.GetSelectedRow(ll_FilaIni + 1)
				LOOP
				
				ll_FilaIni 	=	ll_FilaSel + 1
			
				DO WHILE ll_FilaIni <> al_fila + 1
					dw_2.SelectRow(ll_FilaIni, Not dw_2.IsSelected(al_fila))
					ll_FilaIni 	++
				LOOP
			END IF
		END IF
		
	CASE "Control"
		IF al_fila > 0 THEN dw_2.SelectRow(al_fila, Not dw_2.IsSelected(al_fila))
		
	CASE ELSE
		dw_2.SelectRow(0, False)
		dw_2.SelectRow(al_fila, True)
			
END CHOOSE

dw_2.SetRedraw(True)
end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

//ldt_FechaHora		=	F_FechaHora()

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_2.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_2.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

event open;x				= 0
y				= 0

im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
This.Icon									=	Gstr_apl.Icono

dw_2.SetTransObject(sqlca)
dw_2.Modify("datawindow.message.title='Error '+ is_titulo")
dw_2.SetRowFocusIndicator(Hand!)
dw_2.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
buscar			= "Código:Ncodigo,Descripción:Sconcepto"
ordenar			= "Código:codigo,Descripción:concepto"
is_ultimacol	= "columna"


//ii_tipo	=	Integer(Message.StringParm)

dw_3.SetTransObject(sqlca)
dw_3.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_3.InsertRow(0)
dw_3.SetItem(1,"clie_codigo", gi_codexport)

dw_4.SetTransObject(sqlca)
dw_4.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_4.InsertRow(0)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)


iuo_Planta				=	Create uo_plantadesp
iuo_ClienteProd		=	Create uo_clientesprod
end event

on w_mant_elimina_compacto_pc.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.em_numero=create em_numero
this.st_3=create st_3
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_2=create dw_2
this.cb_1=create cb_1
this.cb_2=create cb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_numero
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.dw_3
this.Control[iCurrent+6]=this.dw_4
this.Control[iCurrent+7]=this.dw_2
this.Control[iCurrent+8]=this.cb_1
this.Control[iCurrent+9]=this.cb_2
end on

on w_mant_elimina_compacto_pc.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_2)
destroy(this.cb_1)
destroy(this.cb_2)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_2.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]),long(istr_mant.argumento[3]))
	
	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_2.SetRow(1)
		dw_2.SetFocus()
		//pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= 	True
		pb_grabar.Enabled		= 	True
		cb_1.Enabled			=	True
		cb_2.Enabled			=	True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
//	pb_insertar.Enabled = True
END IF














end event

event ue_imprimir;//SetPointer(HourGlass!)
//
//Long		ll_Filas
//str_info	lstr_info
//
//lstr_info.titulo	= "DETALLE DE PALLET"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_rotulacion_pallet"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//ll_Filas	=	vinf.dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))
//
//IF ll_Filas = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF ll_Filas = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

event ue_guardar;IF dw_2.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_validaregistro;call super::ue_validaregistro;//Integer	li_cont
//String	ls_mensaje, ls_colu[]
//Long		ll_Fila = 1
//
//		
//IF dw_1.rowcount() = 0 THEN
//	pb_grabar.Enabled	=	False 
//ELSE	
//	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nNo existen mas Registros"
//		ls_colu[li_cont]	= "paen_numero"
//	END IF
//
//	IF IsNull(dw_1.Object.pafr_varrot[il_fila]) OR dw_1.Object.pafr_varrot[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Variedad Rotulada"
//		ls_colu[li_cont]	= "pafr_varrot"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_prdrot[il_fila]) OR dw_1.Object.pafr_prdrot[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Productor Rotulado"
//		ls_colu[li_cont]	= "pafr_prdrot"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_calrot[il_fila]) OR dw_1.Object.pafr_calrot[il_fila] = ''	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Calibre Rotulado"
//		ls_colu[li_cont]	= "pafr_calrot"
//	END IF
//	
//	IF IsNull(dw_1.Object.pafr_huert4[il_fila]) OR dw_1.Object.pafr_huert4[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Predio Rotulado"
//		ls_colu[li_cont]	= "pafr_huert4"
//	END IF	
//
//	IF IsNull(dw_1.Object.pafr_cuart4[il_fila]) OR dw_1.Object.pafr_cuart4[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Cuartel Rotulado"
//		ls_colu[li_cont]	= "pafr_cuart4"
//	END IF	
//
//	IF IsNull(dw_1.Object.pafr_rotpak[il_fila]) OR dw_1.Object.pafr_rotpak[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de Packing Rotulado"
//		ls_colu[li_cont]	= "pafr_rotpak"
//	END IF	
//	
//	IF dw_1.Object.pafr_fecrot[il_fila] = Date('19000101')	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nFalta ingreso de fecha embalaje Rotulada"
//		ls_colu[li_cont]	= "pafr_fecrot"
//	END IF		
//	
//	IF li_cont > 0 THEN
//		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
//		dw_1.SetColumn(ls_colu[1])
//		dw_1.SetFocus()
//	
////		ii_contador = 1
////		
////		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
////			ii_contador++
////		loop 
////		ii_contador --
////		dw_1.SetRow(ii_contador)
////		Message.DoubleParm = -1
//	END IF
//END IF
//
//
//
//
//
end event

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila

		
IF dw_2.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE
	
	FOR ll_Fila = 1 TO dw_2.RowCount()
		
		IF dw_2.IsSelected(ll_Fila) THEN
				 
				dw_2.Object.capr_estado[ll_Fila] = 3
				dw_2.Object.capr_numpal[ll_Fila] = 0
				 
		END IF
	
	NEXT	
	
END IF





end event

type st_encabe from w_mant_directo`st_encabe within w_mant_elimina_compacto_pc
integer x = 87
integer y = 20
integer width = 2912
integer height = 356
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_elimina_compacto_pc
integer x = 3113
integer y = 428
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;dw_3.Enabled			=	TRUE
dw_4.Enabled			=	TRUE
em_numero.Enabled		=	TRUE

dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetFocus()
dw_3.Object.clie_codigo.Background.Color=	RGB(255, 255, 255)
dw_3.SetItem(1,"clie_codigo", gi_codexport)

dw_4.Reset()
dw_4.InsertRow(0)
dw_4.Object.plde_codigo.Background.Color=	RGB(255, 255, 255)
dw_4.SetItem(1,"plde_codigo", gi_codplanta)

dw_2.Reset()
cb_1.Enabled			=	False
cb_2.Enabled			=	False


em_numero.BackColor=	RGB(255, 255, 255)
em_numero.text = ""

istr_mant.argumento[1] =String(gi_codexport)
istr_mant.argumento[2] =String(gi_codplanta)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_elimina_compacto_pc
integer x = 3109
integer y = 88
integer taborder = 50
end type

event pb_lectura::clicked;integer li_nombrevari


IF IsNull(dw_3.Object.clie_codigo[1]) OR dw_3.Object.clie_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Cliente Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(dw_4.Object.plde_codigo[1]) OR  dw_4.Object.plde_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Planta Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(integer(em_numero.text)) OR integer(em_numero.text) = 0 THEN
	MessageBox("Atención","Debe Seleccionar Nº de Pallet Previamente",Exclamation!)
	RETURN
	em_numero.SetFocus()
ELSE
	dw_3.Enabled	=	FALSE
	dw_4.Enabled	=	FALSE
	em_numero.Enabled	=	FALSE
	dw_3.Object.clie_codigo.Background.Color=	RGB(166,180,210)
	dw_4.Object.plde_codigo.Background.Color=	RGB(166,180,210)
	em_numero.BackColor=	RGB(166,180,210)
	Parent.PostEvent("ue_recuperadatos")
	
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_elimina_compacto_pc
boolean visible = false
integer x = 3109
integer y = 788
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_elimina_compacto_pc
boolean visible = false
integer x = 3109
integer y = 608
integer taborder = 80
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_elimina_compacto_pc
integer x = 3109
integer y = 1532
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_elimina_compacto_pc
boolean visible = false
integer x = 3109
integer y = 1148
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_elimina_compacto_pc
integer x = 3109
integer y = 968
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_elimina_compacto_pc
boolean visible = false
integer x = 87
integer y = 496
integer width = 2912
integer height = 1188
integer taborder = 60
boolean enabled = false
string dataobject = "dw_mues_variedadrotulada"
boolean hscrollbar = true
end type

event dw_1::clicked;//Integer	li_cont
//String	ls_mensaje, ls_colu[]
//Long		ll_Fila = 1
//
//IF Row > 0 THEN
//	il_fila = Row
//	This.SelectRow(0,False)
//	This.SetRow(il_fila)
//END IF
//
//	
//IF dw_1.rowcount() = 0 THEN
//	pb_grabar.Enabled	=	False 
//ELSE	
//	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
//		li_cont ++
//		ls_mensaje			= ls_mensaje + "~nNo existen mas registros"
//		ls_colu[li_cont]	= "paen_numero"
//	END IF
//	
//	IF IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0	THEN
//	END IF
//	
//	IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0	THEN
//	END IF
//	
//	IF li_cont > 0 THEN
//		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
//		dw_1.SetColumn(ls_colu[1])
//		dw_1.SetFocus()
//	
//		ii_contador = 1
//		
//		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
//			ii_contador++
//		loop 
//		ii_contador --
//		dw_1.SetRow(ii_contador)
//		Message.DoubleParm = -1
//	END IF
//END IF
//
//IF dw_1.rowcount() > 0 THEN
//	IF dw_1.Object.Paen_numero[il_fila] = 0 THEN
//		dw_1.SetRow(ii_contador)
//	END IF
//END IF
//
//
//
////CHOOSE CASE ls_Columna	
////		
////	CASE "pafr_varrot"
////		istr_mant.Argumento[2]	=	Data	
////			dw_1.SetItem(il_fila,"vari_nombre_1",f_variedadnom(integer(data)))
////			dw_1.SetItem(il_fila,"pafr_calrot",ls_null)
////			IF NoExisteVariedad(ls_Columna,Data)	THEN
////			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
////			RETURN 1
////		END IF
////	
////END CHOOSE
////
////
////
////
////
end event

event dw_1::itemchanged;call super::itemchanged;//String ls_Columna, ls_null, ls_varnom
//Integer	li_Nula 
//
//
//SetNull(li_Nula)
//SetNull(ls_null)
//
//dw_1.AcceptText()
//
//ls_Columna	=	Dwo.Name
//
//CHOOSE CASE ls_Columna	
//		
//	CASE "pafr_varrot"
//		istr_mant.Argumento[2]	=	Data	
//			//dw_1.SetItem(il_fila,"vari_nombre_rotula",f_variedadnom(integer(data)))
//			IF NoExisteVariedad(ls_Columna,Data)	THEN
//				dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
//				RETURN 1
//			
//			END IF
//	
//	CASE 'emba_codigo'
//		IF existeembalaje(Data,dw_3.Object.clie_codigo[1])	THEN
//			dw_1.SetItem(il_Fila,'emba_codigo',String(li_Nula))
//			RETURN 1
//			
//		END IF
//		
//		
//	END CHOOSE	
//
//
//
//
end event

event dw_1::buttonclicked;call super::buttonclicked;//CHOOSE CASE dwo.name		
//	CASE "buscavariedad"
//			buscavariedad()
//			
//	
//END CHOOSE
//
//
end event

event dw_1::rowfocuschanged;//integer li_Fila
//
//li_Fila = dw_1.RowCount()
//
//IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
//	ib_datos_ok = False
//ELSE
//	il_fila				=	GetRow()
//	pb_grabar.Enabled	=	True
//END IF
end event

event dw_1::dwnkey;//This.SetRedraw(False)
end event

type st_2 from statictext within w_mant_elimina_compacto_pc
integer x = 626
integer y = 168
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_elimina_compacto_pc
integer x = 626
integer y = 72
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_elimina_compacto_pc
integer x = 965
integer y = 252
integer width = 402
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[3]=em_numero.text

IF existepallet(Long(em_numero.text)) = False THEN
	This.SetFocus()
END IF
end event

type st_3 from statictext within w_mant_elimina_compacto_pc
integer x = 626
integer y = 268
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Nro. Pallet"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_mant_elimina_compacto_pc
integer x = 965
integer y = 56
integer width = 1230
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null

SetNull(ll_null)

IF iuo_ClienteProd.existe(Integer(Data), True, sqlca) THEN
	istr_mant.argumento[1]	=	String(data)
	
ELSE
	This.SetItem(1, "clie_codigo", Integer(ll_null))

	RETURN 1
END IF
	

end event

event itemerror;Return 1
end event

type dw_4 from datawindow within w_mant_elimina_compacto_pc
integer x = 965
integer y = 152
integer width = 965
integer height = 96
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null


SetNull(ll_null)

IF Not iuo_Planta.Existe(Integer(Data), True, sqlca) THEN
	This.SetItem(1, "plde_codigo", Integer(ll_null))

	RETURN 1
ELSE			
	istr_mant.argumento[2]=String(data)
END IF		



end event

event itemerror;Return 1
end event

type dw_2 from datawindow within w_mant_elimina_compacto_pc
integer x = 82
integer y = 444
integer width = 2907
integer height = 1312
integer taborder = 90
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_elimina_compacto_pc"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;String	ls_Tecla, ls_Columna, ls_Orden[] = {" A", " D"}, &
			ls_Columnas[]	=	{"capr_numero", "espe_codigo", &
									"vari_codigo", "prod_codigo", "prod_predio", "prod_cuarte", &
									"emba_codigo", "etiq_codigo","capr_fecemb","capr_calibr"}
//			ls_Ordenam		=	"&capr_numero&embq_codigo_t02&espe_codigo_t03&vari_codigo_t04" + &
//									"&vari_nombre_t05&zona_codigo_t06&prod_codigo_t07&cate_codigo_t08" + &
//									"&pool_calibr_t09&cond_codigo_t10&enva_tipoen_t11&enva_codigo_t12&etiq_codigo_t13"
									
Long		ll_Fila, ll_Anexo
Integer	li_Posicion

IF Row > 0 THEN
   il_Fila	=	Row

	IF KeyDown(KeyShift!) THEN
		ls_tecla	=	"Shift"
	ELSEIF KeyDown(KeyControl!) THEN
		ls_tecla	=	"Control"
	END IF
	
	wf_Selecciona(ls_Tecla, Row)
END IF
end event

type cb_1 from commandbutton within w_mant_elimina_compacto_pc
integer x = 82
integer y = 372
integer width = 256
integer height = 72
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Todos"
end type

event clicked;dw_2.SelectRow(0, True)
end event

type cb_2 from commandbutton within w_mant_elimina_compacto_pc
integer x = 2743
integer y = 372
integer width = 261
integer height = 72
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Ninguno"
end type

event clicked;dw_2.SelectRow(0, False)
end event

