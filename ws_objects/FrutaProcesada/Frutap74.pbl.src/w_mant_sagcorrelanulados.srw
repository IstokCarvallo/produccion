$PBExportHeader$w_mant_sagcorrelanulados.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_sagcorrelanulados from w_mant_directo
end type
type st_1 from statictext within w_mant_sagcorrelanulados
end type
type dw_plantadesp from datawindow within w_mant_sagcorrelanulados
end type
type pb_recupera from picturebutton within w_mant_sagcorrelanulados
end type
type st_2 from statictext within w_mant_sagcorrelanulados
end type
type ddlb_tipocorrelativosag from dropdownlistbox within w_mant_sagcorrelanulados
end type
end forward

global type w_mant_sagcorrelanulados from w_mant_directo
integer x = 155
integer y = 156
integer width = 3666
integer height = 2004
string title = "PROCESO ANULACION CORRELATIVOS SAG"
event ue_validaborrar ( )
st_1 st_1
dw_plantadesp dw_plantadesp
pb_recupera pb_recupera
st_2 st_2
ddlb_tipocorrelativosag ddlb_tipocorrelativosag
end type
global w_mant_sagcorrelanulados w_mant_sagcorrelanulados

type variables
Integer ii_pallet, ii_nuevo, ii_tipo, ii_contador, ii_tipoplanilla
String	is_report

DataWindowChild	idwc_planta




end variables

forward prototypes
public function boolean noexisteplanta (integer ai_planta)
public function boolean duplicado (string valor)
end prototypes

public function boolean noexisteplanta (integer ai_planta);Integer	li_cont
Boolean	lb_retorna = True


SELECT	count(*) INTO :li_cont
FROM    dbo.plantadesp
WHERE	  plde_codigo = :ai_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla Plantas")
	lb_Retorna = False
	
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "Código de Planta No Existe, Ingrese otro Código.", &
	Exclamation!, OK!)
	lb_Retorna = False
END IF

Return lb_retorna
end function

public function boolean duplicado (string valor);Long		ll_fila
Integer	ll_numero


ll_numero	=	Long(valor)

ll_fila	= dw_1.Find("scoa_numero = " + String(ll_numero), + &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

event open;x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 2500
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
								
/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
buscar			= "Código:Ncodigo,Descripción:Sconcepto"
ordenar			= "Código:codigo,Descripción:concepto"
is_ultimacol	= "columna"

dw_plantadesp.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve()
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1, "plde_codigo", gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_CodPlanta)

istr_mant.dw				=	dw_1
end event

on w_mant_sagcorrelanulados.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_plantadesp=create dw_plantadesp
this.pb_recupera=create pb_recupera
this.st_2=create st_2
this.ddlb_tipocorrelativosag=create ddlb_tipocorrelativosag
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_plantadesp
this.Control[iCurrent+3]=this.pb_recupera
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.ddlb_tipocorrelativosag
end on

on w_mant_sagcorrelanulados.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_plantadesp)
destroy(this.pb_recupera)
destroy(this.st_2)
destroy(this.ddlb_tipocorrelativosag)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila, respuesta
Integer	li_fila

DO
	
	ll_fila	= dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))

	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
	pb_insertar.Enabled = True
END IF

end event

event ue_imprimir;SetPointer(HourGlass!)

Long		ll_Filas
str_info	lstr_info

lstr_info.titulo	= "CORRELATIVO SAG ANULADOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_saganulados"

vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))

IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

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

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		li_Fila = 1

FOR li_Fila = 1 TO dw_1.RowCount()
	IF IsNull(dw_1.Object.scoa_numero[li_Fila]) THEN
		MessageBox("Atención", "Número del Correlativo No Puede Ser Nulo.", &
						Exclamation!, Ok!)
		Message.DoubleParm = -1
		Return
	END IF
NEXT	

FOR li_Fila = 1 TO dw_1.RowCount()
	IF IsNull(dw_1.Object.plde_codigo[li_Fila]) THEN
		dw_1.Object.plde_codigo[li_Fila] = dw_plantadesp.Object.plde_codigo[1]
		dw_1.Object.scoa_tipoco[li_Fila] = ii_tipoplanilla
	END IF
NEXT	




end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.scoa_fecanu[il_fila] = Today()
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_sagcorrelanulados
integer x = 59
integer y = 28
integer width = 3104
integer height = 316
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_sagcorrelanulados
integer x = 3278
integer y = 396
integer taborder = 50
end type

event pb_nuevo::clicked;call super::clicked;dw_plantadesp.Reset()
dw_plantadesp.Object.plde_codigo.Background.Color=	RGB(255, 255, 255)
dw_plantadesp.Enabled	= True
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1, "plde_codigo", gi_CodPlanta)

ddlb_TipoCorrelativoSag.Enabled				=	TRUE

dw_plantadesp.SetFocus()



end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_sagcorrelanulados
integer x = 3278
integer y = 96
integer taborder = 30
end type

event pb_lectura::clicked;Long		ll_fila

dw_plantadesp.Enabled		=	FALSE

dw_1.SettransObject(sqlca)

IF ii_tipoplanilla = 0 THEN
	MessageBox("Atención", "Falta Selecionar Tipo Planilla.", &
	Exclamation!, OK!)
	Return
END IF	

istr_mant.argumento[2] = String(ii_tipoplanilla)

ddlb_TipoCorrelativoSag.Enabled				=	FALSE

dw_plantadesp.Object.plde_codigo.Background.Color	=	RGB(192, 192, 192)

Parent.PostEvent("ue_recuperadatos")
	


	
		




end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_sagcorrelanulados
integer x = 3278
integer y = 756
integer taborder = 70
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_sagcorrelanulados
integer x = 3278
integer y = 576
integer taborder = 60
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_sagcorrelanulados
integer x = 3278
integer y = 1660
integer taborder = 110
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_sagcorrelanulados
integer x = 3278
integer y = 1116
integer taborder = 90
boolean enabled = true
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_sagcorrelanulados
integer x = 3278
integer y = 936
integer taborder = 80
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_sagcorrelanulados
integer x = 59
integer y = 360
integer width = 3109
integer height = 1400
integer taborder = 40
string dataobject = "dw_mant_sagcorrelanulados"
boolean hscrollbar = true
end type

event dw_1::rowfocuschanged;integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event dw_1::itemchanged;call super::itemchanged;Integer		li_null

SetNull(li_null)

CHOOSE CASE dwo.Name
	
	CASE "scoa_numero"
		IF Duplicado(data) THEN
			This.SetItem(il_fila, "scoa_numero", Long(li_null))
			RETURN 1
		END IF

END CHOOSE

end event

type st_1 from statictext within w_mant_sagcorrelanulados
integer x = 302
integer y = 80
integer width = 315
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta "
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_mant_sagcorrelanulados
integer x = 873
integer y = 64
integer width = 1467
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer 	ll_null

SetNull(ll_null)

IF Not noexisteplanta(long(Data)) THEN
	dw_plantadesp.SetItem(1, "plde_codigo", Long(ll_null))
	RETURN 1
ELSE
	istr_mant.argumento[1]	=	String(data)
	
END IF
	

end event

event itemerror;Return 1
end event

type pb_recupera from picturebutton within w_mant_sagcorrelanulados
string tag = "Caja a Caja"
boolean visible = false
integer x = 3278
integer y = 1388
integer width = 233
integer height = 196
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\Apuntee.bmp"
string disabledname = "\Desarrollo 12\Imagenes\Botones\Apunted.bmp"
alignment htextalign = left!
end type

event clicked;//
end event

type st_2 from statictext within w_mant_sagcorrelanulados
integer x = 302
integer y = 204
integer width = 498
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Correlativo "
boolean focusrectangle = false
end type

type ddlb_tipocorrelativosag from dropdownlistbox within w_mant_sagcorrelanulados
integer x = 873
integer y = 208
integer width = 1742
integer height = 400
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string item[] = {"1 Despachos...Productos Agrícolas de Exportación Certificados","2 Despachos...Productos Agr.Export. Certificados (USDA)","3 Despachos...Fruta a ser Fumigada en U.S.A."}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_tipoplanilla	=	(index)
end event

