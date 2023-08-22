$PBExportHeader$w_mant_mues_ctlcalevalumaducolor.srw
$PBExportComments$Mantenedor de Técnicos
forward
global type w_mant_mues_ctlcalevalumaducolor from w_mant_tabla
end type
type dw_especie from datawindow within w_mant_mues_ctlcalevalumaducolor
end type
type st_1 from statictext within w_mant_mues_ctlcalevalumaducolor
end type
type uo_selespecie from uo_seleccion_especie_mod within w_mant_mues_ctlcalevalumaducolor
end type
end forward

global type w_mant_mues_ctlcalevalumaducolor from w_mant_tabla
integer width = 2455
integer height = 1988
string title = "MAESTRO EVALUACION MADUREZ Y COLOR"
event ue_validaregistro ( )
dw_especie dw_especie
st_1 st_1
uo_selespecie uo_selespecie
end type
global w_mant_mues_ctlcalevalumaducolor w_mant_mues_ctlcalevalumaducolor

type variables

DataWindowChild	 	idwc_especie, idwc_variedad

uo_especie				iuo_especies
integer		ii_especie


end variables

forward prototypes
public function boolean duplicado (string campo, string valor)
end prototypes

public function boolean duplicado (string campo, string valor);Long		ll_fila, ll_Especie, ll_Variedad, ll_Evalua

ll_Evalua		=	dw_1.Object.evmc_codigo[il_fila]

CHOOSE CASE campo
		
	CASE "evmc_codigo"
		ll_Evalua		=	Long(valor)
	
END CHOOSE

ll_fila	= dw_1.Find("evmc_codigo = " + String(ll_Evalua),1, dw_1.RowCount())

IF ll_fila > 0 AND ll_fila <> il_fila THEN
	MessageBox("Atención","Registro ya fue ingresado",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_mant_mues_ctlcalevalumaducolor.create
int iCurrent
call super::create
this.dw_especie=create dw_especie
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_especie
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.uo_selespecie
end on

on w_mant_mues_ctlcalevalumaducolor.destroy
call super::destroy
destroy(this.dw_especie)
destroy(this.st_1)
destroy(this.uo_selespecie)
end on

event open;/*
	istr_mant.argumento[1]	=	Código de Cliente
	istr_mant.argumento[2]	=	Código de Especie
*/

boolean lb_cerrar
x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 1993
im_menu	= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

istr_mant.dw	= dw_1

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	
istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	"0"

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")


buscar	=	"Código:evmc_codigo,Descripción:evmc_descri"
ordenar	=	"Código:evmc_codigo,Descripción:evmc_descri"

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
dw_especie.setfocus()

	ii_Especie = Message.DoubleParm
	
	uo_SelEspecie.Seleccion(False, False)
	
	If ii_Especie <> 0 Then
		//uo_SelEspecie.Bloquea(True)
		uo_SelEspecie.Codigo	=	ii_Especie
		uo_SelEspecie.dw_Seleccion.Object.Codigo[1]=	ii_Especie
		uo_SelEspecie.Enabled = False
		pb_lectura.postevent(Clicked!)
		pb_nuevo.Enabled = False
		pb_lectura.Enabled = False
	End If
End If
end event

event ue_recuperadatos;Long		ll_fila, respuesta
Integer  li_grupo,li_UsAdmi
String   ls_Usuario
ls_Usuario		=	Upper(Gstr_Us.Nombre)
li_UsAdmi		= Integer(Gstr_Us.Administrador)
DO	
	//ll_fila			=	dw_1.Retrieve(dw_Especie.Object.espe_codigo[1])
		ll_fila			=	dw_1.Retrieve(uo_SelEspecie.Codigo)
		
	IF ll_fila 		=	-1 THEN
		respuesta 	=	MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila >= 0 THEN
		
			li_Grupo = BuscaGrupo(ls_Usuario)					
			IF  li_UsAdmi	=	1 OR (li_Grupo = 1) THEN 			
				dw_1.SetFocus()
				il_fila					= 1
				pb_imprimir.Enabled	= True
				pb_insertar.Enabled	= True
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
			ELSE
				pb_insertar.SetFocus()
				pb_insertar.Enabled	= False	
				pb_imprimir.Enabled	= True
			END IF 	
		END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO EVALUACION MADUREZ Y COLOR ESPECIES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalevalumaducolor"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_selespecie.codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
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

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
END IF
end event

event ue_nuevo;IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

il_fila = dw_1.InsertRow(0)

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
dw_1.SetColumn("evmc_codigo")



istr_mant.borra	= False
istr_mant.agrega	= True

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
end event

event ue_antesguardar;Long	ll_fila = 1
Integer	li_cont
String	ls_mensaje, ls_colu[]



IF dw_1.RowCount() > 0 THEN
   FOR ll_fila = 1 TO dw_1.RowCount()
		 IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
			 dw_1.DeleteRow(ll_fila)
		 ELSE
			IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
//				dw_1.setitem(ll_fila,"espe_codigo", integer(istr_mant.argumento[2]))
				dw_1.setitem(ll_fila,"espe_codigo",uo_selespecie.codigo)
						
				IF Isnull(dw_1.Object.evmc_codigo[ll_fila]) OR dw_1.Object.evmc_codigo[ll_fila] = 0 THEN
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nCódigo Evaluación y Madurez"
					ls_colu[li_cont]	= "evmc_codigo"
				END IF
				
				IF Isnull(dw_1.Object.evmc_descri[ll_fila]) OR trim(dw_1.Object.evmc_descri[ll_fila]) = "" THEN
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nDescripción Evaluación y Madurez"
					ls_colu[li_cont]	= "evmc_descri"
				END IF	
			END IF
		 END IF
   NEXT
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF

//FOR ll_Fila	= 1 TO dw_1.RowCount()
//	dw_1.Object.espe_codigo[ll_Fila] = dw_especie.Object.espe_codigo[1]
//NEXT
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_ctlcalevalumaducolor
integer x = 73
integer y = 364
integer width = 1833
integer height = 1356
integer taborder = 40
string dataobject = "dw_mues_ctlcalevalumaducolor"
boolean hscrollbar = true
end type

event dw_1::rowfocuschanged;Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::itemchanged;call super::itemchanged;Integer		li_Null

SetNull(li_Null)

CHOOSE CASE dwo.Name
	
	CASE "evmc_codigo"
		IF Duplicado(dwo.Name, data) THEN
			This.SetItem(row, "evmc_codigo", li_Null)
			RETURN 1
		END IF

END CHOOSE


end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

RETURN 0
end event

event dw_1::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)

RETURN 0
end event

event dw_1::getfocus;//
end event

event dw_1::sqlpreview;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_ctlcalevalumaducolor
integer x = 73
integer width = 1833
integer height = 276
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_ctlcalevalumaducolor
integer x = 2103
integer taborder = 20
boolean enabled = false
end type

event pb_lectura::clicked;call super::clicked;dw_especie.enabled		=	False


end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_ctlcalevalumaducolor
integer x = 2098
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;dw_especie.Reset()

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve(gi_codexport)
dw_especie.InsertRow(0)

//dw_especie.Enabled	= True
//dw_especie.SetFocus()

If ii_especie = 0 Then
	uo_selespecie.Enabled = True
	uo_selespecie.SetFocus()
End If


pb_lectura.Enabled	= False
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0


end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_ctlcalevalumaducolor
integer x = 2098
integer taborder = 30
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_ctlcalevalumaducolor
integer x = 2098
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_ctlcalevalumaducolor
integer x = 2098
integer taborder = 50
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_ctlcalevalumaducolor
integer x = 2098
integer taborder = 60
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_ctlcalevalumaducolor
integer x = 2089
integer taborder = 90
end type

type dw_especie from datawindow within w_mant_mues_ctlcalevalumaducolor
string tag = "muestra lista de especies"
boolean visible = false
integer x = 667
integer y = 156
integer width = 891
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null 
SetNull(li_Null)

iuo_especies		= Create uo_especie

IF iuo_especies.existe(Integer(Data),True,sqlca)  Then	
	pb_lectura.setfocus()
	pb_lectura.Enabled		=	True
	pb_lectura.Setfocus()
ELSE
	This.SetItem(1,"espe_codigo",li_null)
	RETURN 1 
	pb_lectura.Enabled		=	False		
END IF
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;pb_lectura.Setfocus()
end event

type st_1 from statictext within w_mant_mues_ctlcalevalumaducolor
integer x = 366
integer y = 160
integer width = 274
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie_mod within w_mant_mues_ctlcalevalumaducolor
event destroy ( )
integer x = 640
integer y = 136
integer width = 1221
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;pb_lectura.Enabled = True


end event

