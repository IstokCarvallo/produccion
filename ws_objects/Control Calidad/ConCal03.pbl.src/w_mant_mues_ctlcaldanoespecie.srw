$PBExportHeader$w_mant_mues_ctlcaldanoespecie.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_mues_ctlcaldanoespecie from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_ctlcaldanoespecie
end type
type dw_especie from datawindow within w_mant_mues_ctlcaldanoespecie
end type
end forward

global type w_mant_mues_ctlcaldanoespecie from w_mant_directo
integer width = 3470
integer height = 1860
st_1 st_1
dw_especie dw_especie
end type
global w_mant_mues_ctlcaldanoespecie w_mant_mues_ctlcaldanoespecie

type variables
uo_especie						iuo_especies	
uo_grabatablas					iuo_grabatablas
DataWindowChild	 			idwc_especie, idwc_familia,idwc_subfamilia
w_busc_ctlcalsubfamilia 		w_busca
integer 							ii_cod_sfam,ii_codfam

end variables

forward prototypes
public function boolean duplicado (string is_columna, string is_valor)
public subroutine wf_replicacion ()
end prototypes

public function boolean duplicado (string is_columna, string is_valor);Long		ll_fila
Integer	li_codigo

li_codigo	=	dw_1.Object.ccda_secuen[il_fila]

CHOOSE CASE is_columna
	CASE "ccda_secuen"
		li_codigo	=	Integer(is_valor)

END CHOOSE

ll_fila	= dw_1.Find("ccda_secuen = " + String(li_codigo), + &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine wf_replicacion ();Integer	ll_fila
ll_fila = dw_1.GetNextModified(ll_fila, Primary!)

IF ll_fila > 0 THEN
	IF iuo_grabatablas.existereplicatablas(gi_CodExport)  THEN
		iuo_grabatablas.replicatabla_danoespecie(dw_1)
	END IF	
END IF


end subroutine

on w_mant_mues_ctlcaldanoespecie.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_especie=create dw_especie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_especie
end on

on w_mant_mues_ctlcaldanoespecie.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_especie)
end on

event open;call super::open;dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)

setnull(istr_mant.Argumento[3])

dw_1.GetChild("ccfa_codigo", idwc_familia)

iuo_grabatablas		=	CREATE uo_grabatablas

idwc_familia.SetTransObject(SQLCA)
idwc_familia.Retrieve()

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

buscar	=	"Familia:Sccfa_descrip,Subfamilia:Sccsf_descrip,Secuencia:Nccda_secuen"
            
//ordenar	=	"Familia:ccfa_descrip,Subfamilia:ccsf_descrip,Detalle:ccde_descip,Secuencia:ccda_secuen"
   ordenar	=	"Familia:ccfa_codigo_t,Subfamilia:ccsf_descrip_t,Secuencia:ccda_secuen_t,Detalle:ccda_descri_t"


dw_especie.setfocus() 

end event

event ue_recuperadatos;Long		ll_fila, respuesta
Integer  li_grupo,li_UsAdmi
String   ls_Usuario
ls_Usuario		=	Upper(Gstr_Us.Nombre)
li_UsAdmi		= Integer(Gstr_Us.Administrador)

IF isnull(istr_mant.argumento[2]) or istr_mant.argumento[2] = '' THEN 
	MessageBox("Advertencia","Debe Seleccionar Una Especie",StopSign!)
	RETURN
END IF

DO
	ll_fila			=	dw_1.Retrieve(Integer(istr_mant.argumento[2]))
		
	IF ll_fila 		=	-1 THEN
		respuesta 	=	MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila >= 0 THEN
		//IF dw_1.Rowcount() > 0 THEN	
			li_Grupo = BuscaGrupo(ls_Usuario)
			IF  li_UsAdmi	=	1 OR (li_Grupo = 1) THEN
				dw_1.SetFocus()
				il_fila					= 1
				pb_imprimir.Enabled	= True
				pb_insertar.Enabled	= True
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
			ELSE
				//pb_insertar.Enabled	= True
				pb_insertar.SetFocus()
				pb_insertar.Enabled	= False
				pb_imprimir.Enabled	= True
			END IF
		//END IF 
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir;call super::ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info


lstr_info.titulo	= "DEFECTOS DE CALIDAD"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcaldanoespecie"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[2]))

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

event ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]


Long	ll_fila = 1

DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP

IF dw_1.RowCount() = 0 THEN
	pb_Grabar.Enabled		=	False
	pb_Eliminar.Enabled	=	False
	pb_Imprimir.Enabled	=	False
ELSE
	IF Isnull(dw_1.Object.ccfa_codigo[il_fila]) OR dw_1.Object.ccfa_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nFamilia"
		ls_colu[li_cont]	= "ccfa_codigo"
	END IF
	
	IF Isnull(dw_1.Object.ccsf_codigo[il_fila]) OR dw_1.Object.ccsf_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nSub Familia"
		ls_colu[li_cont]	= "ccsf_codigo"
	END IF
	
	IF Isnull(dw_1.Object.ccda_secuen[il_fila]) OR dw_1.Object.ccda_secuen[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo"
		ls_colu[li_cont]	= "ccda_secuen"
	END IF
	
	IF Isnull(dw_1.Object.ccda_descri[il_fila]) OR trim(dw_1.Object.ccda_descri[il_fila]) = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nDescripción"
		ls_colu[li_cont]	= "ccda_descri"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	END IF	
END IF




end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

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

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcaldanoespecie
integer width = 2903
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcaldanoespecie
integer x = 3150
integer y = 412
end type

event pb_nuevo::clicked;call super::clicked;pb_eliminar.enabled  = false
pb_imprimir.enabled = false
pb_insertar.enabled = false
pb_grabar.enabled = false


dw_especie.scrolltorow(dw_especie.InsertRow(0))
istr_mant.argumento[2]=''


end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcaldanoespecie
integer x = 3154
integer y = 108
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcaldanoespecie
integer x = 3150
integer y = 772
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcaldanoespecie
integer x = 3145
integer y = 592
end type

event pb_insertar::clicked;call super::clicked;dw_1.SetColumn(1)
dw_1.SetFocus()

end event

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcaldanoespecie
integer x = 3150
integer y = 1516
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcaldanoespecie
integer x = 3159
integer y = 1132
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcaldanoespecie
integer x = 3150
integer y = 952
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcaldanoespecie
integer width = 2903
integer height = 1280
string dataobject = "dw_mues_ctlcaldanoespecie"
boolean hscrollbar = true
end type

event dw_1::buttonclicked;call super::buttonclicked;istr_mant.Argumento[3] = string(dw_1.Object.ccfa_codigo[row])
IF (isnull(istr_mant.Argumento[3]) OR istr_mant.Argumento[3] = '')  THEN RETURN
	  
	OpenWithParm(w_busca, istr_mant)
	
	istr_mant	= Message.PowerObjectParm

IF  (isnull(istr_mant.Argumento[4]) OR istr_mant.Argumento[4] = '')  AND & 
     (isnull(istr_mant.Argumento[5]) OR istr_mant.Argumento[5] = '')  THEN RETURN
	
     ii_cod_sfam = integer(istr_mant.Argumento[4])
	  
	dw_1.setitem(row,"ccsf_codigo",ii_cod_sfam)
	dw_1.setitem(row,"ccsf_descrip",istr_mant.Argumento[5])
	dw_1.SetColumn("ccda_secuen")
	dw_1.SetFocus()

	

end event

event dw_1::itemchanged;call super::itemchanged;Long		ll_null

SetNull(ll_null)

CHOOSE CASE dwo.Name
	
	CASE "ccfa_codigo"
		ii_codfam = integer(data)
		istr_mant.Argumento[3] = data
		this.setitem(row,"espe_codigo",Integer(istr_mant.argumento[2]))
		
	CASE "ccda_secuen"
		IF duplicado("ccda_secuen",data) THEN
			 this.Object.ccda_secuen[row] = ll_null
			 this.SetColumn("ccda_secuen")
		      this.SetFocus()
			 return 1
		END IF
		
END CHOOSE
end event

event dw_1::clicked;call super::clicked;IF Row > 0 THEN
	This.SelectRow(0,False)
	This.SelectRow(Row,True)
	This.SetRow(Row)
END IF

/*
Para que funcione este ordenamiento los títulos deben tener el nombre
de la columna y terminacion "_t", de lo contrario no funcionará
*/
String	ls_old_sort, ls_column, ls_color_old
Char		lc_sort

IF IsNull(dwo) THEN RETURN

If Right(dwo.Name,2) = '_t' Then
	ls_column	= Left (dwo.Name, Len(String(dwo.Name)) - 2)
	ls_old_sort	= This.Describe("Datawindow.Table.sort")
	ls_color_old	=This.Describe(ls_Column + "_t.Color")

	If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
		lc_sort = Right(ls_old_sort, 1)
		If lc_sort = 'A' Then
			lc_sort = 'D'
		Else
			lc_sort = 'A'
		End If
		This.SetSort(ls_column+" "+lc_sort)
	Else
		This.SetSort(ls_column+" A")
		This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + ls_color_old)
	End If
	
//	This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 0, 255)))
	
	This.Sort()
End If

end event

type st_1 from statictext within w_mant_mues_ctlcaldanoespecie
integer x = 832
integer y = 112
integer width = 238
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_mant_mues_ctlcaldanoespecie
string tag = "muestra lista de especies"
integer x = 1175
integer y = 104
integer width = 882
integer height = 100
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null 
SetNull(li_Null)

iuo_especies		= Create    uo_especie

IF iuo_especies.existe(Integer(Data),true,sqlca)  Then	
	pb_lectura.setfocus()
	istr_mant.argumento[2]	=	Data	
	pb_lectura.Enabled		=	True	
	pb_lectura.Setfocus()
ELSE
	This.SetItem(1,"espe_codigo",li_null)
	pb_lectura.Enabled		=	False		
	RETURN 1 
	
END IF
end event

event itemerror;RETURN 1
end event

event itemfocuschanged;pb_lectura.Setfocus()
end event

