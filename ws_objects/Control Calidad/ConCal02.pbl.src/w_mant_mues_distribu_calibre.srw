$PBExportHeader$w_mant_mues_distribu_calibre.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_mues_distribu_calibre from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_distribu_calibre
end type
type st_2 from statictext within w_mant_mues_distribu_calibre
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_mant_mues_distribu_calibre
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_distribu_calibre
end type
type st_3 from statictext within w_mant_mues_distribu_calibre
end type
end forward

global type w_mant_mues_distribu_calibre from w_mant_tabla
string tag = "Muestra daños por especie de tabla CTLCALDANOESPECIE"
integer width = 2405
integer height = 1912
string title = "MAESTRO DISTRIBUCION CALIBRE"
st_1 st_1
st_2 st_2
uo_selvariedad uo_selvariedad
uo_selespecie uo_selespecie
st_3 st_3
end type
global w_mant_mues_distribu_calibre w_mant_mues_distribu_calibre

type variables
w_mant_deta_distribu_calibre  iw_mantencion

Long		li_Especie
end variables

on w_mant_mues_distribu_calibre.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.uo_selvariedad=create uo_selvariedad
this.uo_selespecie=create uo_selespecie
this.st_3=create st_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.uo_selvariedad
this.Control[iCurrent+4]=this.uo_selespecie
this.Control[iCurrent+5]=this.st_3
end on

on w_mant_mues_distribu_calibre.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selvariedad)
destroy(this.uo_selespecie)
destroy(this.st_3)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecie.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	li_Especie = Message.DoubleParm
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	
	If li_Especie <> 0 Then
		uo_SelEspecie.Bloquear(True)
		uo_SelEspecie.Inicia(li_Especie)
		uo_SelVariedad.Filtra(li_Especie)
	End If
	
	istr_mant.argumento[1]	=	String(gi_CodExport)
	istr_mant.argumento[2]	=	"0"
	
	buscar	=	"Orden:Nccdc_ordena,Calibres:Sccdc_calibr,Gramos:Sccdc_gramos,Milimetros:Scccdc_milime"
	ordenar	=	"Orden:Nccdc_ordena,Calibres:ccdc_calibr,Gramos:ccdc_gramos,Milimetros:cccdc_milim"
End If
end event

event ue_recuperadatos;Long		ll_fila, respuesta
Integer  li_grupo,li_UsAdmi
String   ls_Usuario

ls_Usuario	=	Upper(Gstr_Us.Nombre)
li_UsAdmi		= Integer(Gstr_Us.Administrador)

DO	
	ll_fila			=	dw_1.Retrieve(uo_SelEspecie.Codigo, uo_SelVariedad.Codigo)
		
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

event ue_borrar();IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant) 

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
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
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DISTRIBUCION CALIBRE"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_distribu_calibre"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelEspecie.Codigo, uo_SelVariedad.Codigo)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_Mant.Argumento[2] = String(uo_SelEspecie.Codigo)
	istr_Mant.Argumento[3] = String(uo_Selvariedad.Codigo)

	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_Mant.Argumento[2] = String(uo_SelEspecie.Codigo)
istr_Mant.Argumento[3] = String(uo_Selvariedad.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	ll_Fila, ll_Secuen

SELECT	IsNull(MAX(ccdc_secuen),0) + 1
	INTO	:ll_Secuen
	FROM	dbo.ctlcaldistcalibre
	WHERE	espe_codigo	=	:uo_SelEspecie.Codigo
	AND		vari_codigo	=	:uo_SelVariedad.Codigo
	USING SQLCA;
	
If SQLCA.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLCA, "Lectura de Tabla Distribucion de Calibres")
	Message.DoubleParm = -1
	Return
End If

For ll_Fila = 1 TO dw_1.RowCount()
	If dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModIfied! Then
		dw_1.Object.ccdc_secuen[ll_Fila] = ll_Secuen
		ll_Secuen++
	End If 
Next

dw_1.SetSort("ccdc_ordena asc")
dw_1.Sort()

For ll_Fila = 1 TO dw_1.RowCount()
	dw_1.Object.ccdc_ordena[ll_Fila] = ll_Fila*10
Next
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_distribu_calibre
integer y = 508
integer width = 1563
integer height = 1208
integer taborder = 60
string dataobject = "dw_mues_distribu_calibre"
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

event dw_1::sqlpreview;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_distribu_calibre
integer y = 72
integer width = 1696
integer height = 380
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_distribu_calibre
integer x = 1984
integer y = 152
end type

event pb_lectura::clicked;call super::clicked;uo_SelEspecie.Bloquear(True)
uo_SelVariedad.Bloquear(True)

end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_distribu_calibre
integer x = 1979
integer y = 448
integer taborder = 30
end type

event pb_nuevo::clicked;call super::clicked;If li_Especie <> 0 Then
	uo_SelVariedad.Bloquear(False)
	uo_SelVariedad.LimpiarDatos()
Else
	uo_SelEspecie.Bloquear(False)
	uo_SelEspecie.LimpiarDatos()
	uo_SelVariedad.Bloquear(False)
	uo_SelVariedad.LimpiarDatos()
End If

pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0


end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_distribu_calibre
integer x = 1979
integer y = 628
integer taborder = 80
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_distribu_calibre
integer x = 1979
integer y = 808
integer taborder = 90
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_distribu_calibre
integer x = 1979
integer y = 988
integer taborder = 100
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_distribu_calibre
integer x = 1979
integer y = 1168
integer taborder = 110
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_distribu_calibre
integer x = 1979
integer y = 1552
integer taborder = 120
end type

type st_1 from statictext within w_mant_mues_distribu_calibre
integer x = 137
integer y = 152
integer width = 315
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

type st_2 from statictext within w_mant_mues_distribu_calibre
integer x = 137
integer y = 308
integer width = 315
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
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad_mod within w_mant_mues_distribu_calibre
event destroy ( )
integer x = 453
integer y = 276
integer width = 1033
integer height = 112
integer taborder = 20
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_mant_mues_distribu_calibre
event destroy ( )
integer x = 448
integer y = 148
integer height = 80
integer taborder = 40
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return 

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose	
end event

type st_3 from statictext within w_mant_mues_distribu_calibre
integer x = 1490
integer y = 304
integer width = 183
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean focusrectangle = false
end type

