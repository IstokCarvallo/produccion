$PBExportHeader$w_info_proceso_embalada.srw
forward
global type w_info_proceso_embalada from w_para_informes
end type
type st_2 from statictext within w_info_proceso_embalada
end type
type st_3 from statictext within w_info_proceso_embalada
end type
type st_4 from statictext within w_info_proceso_embalada
end type
type ddlb_proc from dropdownlistbox within w_info_proceso_embalada
end type
type st_1 from statictext within w_info_proceso_embalada
end type
type cb_orden from commandbutton within w_info_proceso_embalada
end type
type em_proc from editmask within w_info_proceso_embalada
end type
type st_5 from statictext within w_info_proceso_embalada
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_proceso_embalada
end type
type uo_selplantas from uo_seleccion_plantas within w_info_proceso_embalada
end type
end forward

global type w_info_proceso_embalada from w_para_informes
integer width = 2464
integer height = 1368
string title = "Informe de Revisión Ingreso de Procesos"
st_2 st_2
st_3 st_3
st_4 st_4
ddlb_proc ddlb_proc
st_1 st_1
cb_orden cb_orden
em_proc em_proc
st_5 st_5
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_info_proceso_embalada w_info_proceso_embalada

type variables
Long	ii_tipo, il_nroproc
end variables

forward prototypes
public function boolean existenroproceso (integer ai_cliente, integer ai_planta, integer ai_tipopro, long al_numero)
end prototypes

public function boolean existenroproceso (integer ai_cliente, integer ai_planta, integer ai_tipopro, long al_numero);Long existe_numero
boolean lb_bolean

//IF ai_tipopro = 5 THEN
//	
//	SELECT dinp_numero
//    INTO :existe_numero  
//    FROM dbo.spro_doctointernopack  
//   WHERE dinp_tipdoc = :ai_tipopro
//	  AND plde_codigo = :ai_planta
////	  AND clie_codigo = :ai_cliente
//	  AND dinp_numero = :al_numero;
//
//  IF sqlca.SQLCode = -1 THEN
//     F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_doctointernopack  ")
//	  lb_bolean = FALSE
//  ELSEIF sqlca.SQLCode = 100 THEN
//	  MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
//	  lb_bolean = FALSE
//  ELSE
//	  lb_bolean = TRUE
//  END IF
//  
//
//ELSEIF  ddlb_proc.text = "Proceso"  THEN
//		
  SELECT orpr_numero  
    INTO :existe_numero  
    FROM dbo.spro_ordenproceso  
   WHERE plde_codigo = :ai_planta
	AND   orpr_tipord = :ai_tipopro
	AND   clie_codigo = :ai_cliente
	AND   orpr_numero = :al_numero;
	
	IF sqlca.SQLCode = -1 THEN
     F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordendeproceso  ")
	  lb_bolean = FALSE 
  ELSEIF sqlca.SQLCode = 100 THEN
	  MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	  lb_bolean = FALSE
  ELSE
	  lb_bolean = TRUE
  END IF

//END IF

RETURN lb_bolean
end function

on w_info_proceso_embalada.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.ddlb_proc=create ddlb_proc
this.st_1=create st_1
this.cb_orden=create cb_orden
this.em_proc=create em_proc
this.st_5=create st_5
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.ddlb_proc
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.cb_orden
this.Control[iCurrent+7]=this.em_proc
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.uo_selcliente
this.Control[iCurrent+10]=this.uo_selplantas
end on

on w_info_proceso_embalada.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.ddlb_proc)
destroy(this.st_1)
destroy(this.cb_orden)
destroy(this.em_proc)
destroy(this.st_5)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gstr_paramplanta.CodigoPlanta)
	
	ddlb_proc.SelectItem("Proceso", 1)
	ddlb_proc.TriggerEvent("SelectionChanged!")
	
	ii_tipo 		= 	4
End if
end event

type pb_excel from w_para_informes`pb_excel within w_info_proceso_embalada
end type

type st_computador from w_para_informes`st_computador within w_info_proceso_embalada
end type

type st_usuario from w_para_informes`st_usuario within w_info_proceso_embalada
end type

type st_temporada from w_para_informes`st_temporada within w_info_proceso_embalada
end type

type p_logo from w_para_informes`p_logo within w_info_proceso_embalada
end type

type st_titulo from w_para_informes`st_titulo within w_info_proceso_embalada
integer width = 1737
string text = "Revisión Ingreso de Procesos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_proceso_embalada
integer x = 2094
integer y = 500
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Long		fila

istr_info.titulo	= "INFORME REVISION PROCESO FRUTA EMBALADA"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

IF ii_tipo <> 5 THEN
	vinf.dw_1.DataObject = "dw_info_revision_proceso_embalada"
	vinf.dw_1.SetTransObject(sqlca)
ELSE
	vinf.dw_1.DataObject = "dw_info_revision_reproceso_embalada"
	vinf.dw_1.SetTransObject(sqlca)
END IF

fila = vinf.dw_1.Retrieve(uo_SelPlantas.Codigo, ii_tipo, il_nroproc, uo_SelCliente.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_proceso_embalada
integer x = 2094
integer y = 816
end type

type st_2 from statictext within w_info_proceso_embalada
integer x = 329
integer y = 632
integer width = 206
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_proceso_embalada
integer x = 329
integer y = 768
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo de Proc."
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_proceso_embalada
integer x = 329
integer y = 896
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Proceso"
boolean focusrectangle = false
end type

type ddlb_proc from dropdownlistbox within w_info_proceso_embalada
integer x = 741
integer y = 740
integer width = 549
integer height = 416
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean sorted = false
boolean hscrollbar = true
string item[] = {"Proceso","Re - Proceso","Re - Embalaje","Pre - Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index 
	CASE 1
		ii_tipo	=	4
		
	CASE 2
		ii_tipo 	=	5
		
	CASE 3
		ii_tipo 	= 	7
		
	CASE 4
		ii_tipo	=	8
		
END CHOOSE
end event

type st_1 from statictext within w_info_proceso_embalada
integer x = 247
integer y = 416
integer width = 1737
integer height = 676
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_orden from commandbutton within w_info_proceso_embalada
integer x = 1161
integer y = 872
integer width = 110
integer height = 84
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;str_busqueda	lstr_busq

lstr_busq.argum[4]= String(uo_SelCliente.Codigo)
lstr_busq.argum[6]= ddlb_proc.text

IF ii_tipo > 4 THEN
	
	lstr_busq.argum[1] = String(ii_tipo)
	
	OpenWithParm(w_busqueda_doctointernopack,lstr_busq)
	
	lstr_busq	= Message.PowerObjectParm

	IF lstr_busq.argum[2] <> "" then
		uo_SelPlantas.Inicia(integer(lstr_busq.argum[1]))
		ddlb_proc.SelectItem(Integer(lstr_busq.argum[2])-3)
		
		em_proc.text 	=	lstr_busq.argum[3]
		ii_tipo 			=	integer(lstr_busq.argum[2])
		il_nroproc 		=	Long(lstr_busq.argum[3])
	end if 	
ELSEIF  ddlb_proc.text = "Proceso" THEN
	lstr_busq.argum[1] = string(uo_SelPlantas.Codigo)
	lstr_busq.argum[2] = "0"
	lstr_busq.argum[3] = "4"
	
	OpenWithParm(w_busc_spro_ordenproceso,lstr_busq)
	lstr_busq	= Message.PowerObjectParm
	IF lstr_busq.argum[1] <> "" then
		uo_SelPlantas.Inicia(integer(lstr_busq.argum[3]))
		ii_tipo 			=	4
		il_nroproc 		=	Long(lstr_busq.argum[6])
	   em_proc.text   =  lstr_busq.argum[6]
	END IF	
END IF

RETURN 0
end event

type em_proc from editmask within w_info_proceso_embalada
integer x = 741
integer y = 868
integer width = 402
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF IsNull(ii_tipo) OR ii_tipo=0 THEN
	Messagebox("Atención","Debe Seleccionar Tipo de Proceso",exclamation!)
   	ddlb_proc.SetFocus()
	This.Text	=	""
	RETURN
ELSE
  	ExisteNroProceso(uo_SelCliente.Codigo, uo_SelPlantas.Codigo,ii_tipo,Long(this.text))
	il_nroproc	=	Long(This.Text)
END IF
end event

type st_5 from statictext within w_info_proceso_embalada
integer x = 329
integer y = 524
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_proceso_embalada
event destroy ( )
integer x = 741
integer y = 492
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_proceso_embalada
event destroy ( )
integer x = 741
integer y = 620
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

