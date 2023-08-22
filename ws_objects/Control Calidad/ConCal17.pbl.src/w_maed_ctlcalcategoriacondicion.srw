$PBExportHeader$w_maed_ctlcalcategoriacondicion.srw
$PBExportComments$Ingresador de antecedentes para Categorias de Condicion
forward
global type w_maed_ctlcalcategoriacondicion from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_ctlcalcategoriacondicion
end type
type st_nota from statictext within w_maed_ctlcalcategoriacondicion
end type
type ddlb_nota from dropdownpicturelistbox within w_maed_ctlcalcategoriacondicion
end type
end forward

global type w_maed_ctlcalcategoriacondicion from w_mant_encab_deta_csd
integer width = 5472
integer height = 2096
string menuname = ""
windowstate windowstate = maximized!
event ue_validapassword ( )
dw_3 dw_3
st_nota st_nota
ddlb_nota ddlb_nota
end type
global w_maed_ctlcalcategoriacondicion w_maed_ctlcalcategoriacondicion

type variables
DataWindowChild	idwc_variedades, idwc_Productores

uo_variedades      	iuo_variedades
uo_productores    	iuo_productor
uo_especie        	iuo_especies
uo_clientesprod		iuo_Cliente
uo_Plantadesp		iuo_Planta

Integer 	ii_sw, il_Nota
end variables

forward prototypes
protected function integer wf_modifica ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine wf_child ()
public function boolean wf_planilla (long planilla)
public function long wf_correlativo (long planta)
public subroutine wf_elimina ()
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Control de Calidad"
istr_mant.Argumento[2]	=	gstr_parlote.paswor

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

protected function integer wf_modifica ();RETURN 1
end function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.plde_codigo.Protect				=	0
	dw_2.Object.coca_numero.Protect				=	0
	dw_2.Object.coca_fechas.Protect				=	0
	dw_2.Object.vari_codigo.Protect				=	0
	dw_2.Object.prod_codigo.Protect				=	0
	dw_2.Object.ticket.Protect						=	0
	
	dw_2.Object.ticket.Color							=	0

	dw_2.Object.plde_codigo.Color		=	0
	dw_2.Object.coca_numero.Color	=	0
	dw_2.Object.coca_fechas.Color	=	0
	dw_2.Object.vari_codigo.Color		=	0
	dw_2.Object.prod_codigo.Color	=	0
	
	dw_2.Object.plde_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.coca_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.coca_fechas.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.vari_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color		=	RGB(255,255,255)
  	pb_buscar.Enabled   									= True
	
Else
 	dw_2.Object.coca_numero.Protect				=	1
	dw_2.Object.plde_codigo.Protect				=	1
	dw_2.Object.coca_fechas.Protect				=	1
	dw_2.Object.vari_codigo.Protect				=	1
	dw_2.Object.prod_codigo.Protect				=	1
	dw_2.Object.ticket.Protect						=	1
	
	dw_2.Object.ticket.Color			=	RGB(166,180,210)	
	
	dw_2.Object.plde_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.coca_numero.Color	=	RGB(255,255,255)
	dw_2.Object.coca_fechas.Color	=	RGB(255,255,255)
	dw_2.Object.vari_codigo.Color		=	RGB(255,255,255)
	dw_2.Object.prod_codigo.Color	=	RGB(255,255,255)
	
	dw_2.Object.plde_codigo.BackGround.Color		=	553648127
	dw_2.Object.coca_numero.BackGround.Color	=	553648127
	dw_2.Object.coca_fechas.BackGround.Color		=	553648127
	dw_2.Object.vari_codigo.BackGround.Color		=	553648127
	dw_2.Object.prod_codigo.BackGround.Color		=	553648127
	pb_buscar.Enabled   									= False

End If
end subroutine

public subroutine habilitaingreso (string ls_columna);Boolean	lb_Estado = True

dw_2.AcceptText()

If ls_Columna <> "coca_fechas" And IsNull(dw_2.Object.coca_fechas[1])  Then
	lb_Estado	=	False
End If

If ls_Columna <> "espe_codigo" And &
	(dw_2.Object.espe_codigo[1]) = 0 Or IsNull(dw_2.Object.espe_codigo[1])  Then
	lb_Estado	=	False
End If

If ls_Columna <> "vari_codigo" And &
	(dw_2.Object.vari_codigo[1]) = 0 Or IsNull(dw_2.Object.vari_codigo[1])  Then
	lb_Estado	=	False
End If

If ls_Columna <> "prod_codigo" And &
	(dw_2.Object.prod_codigo[1]) = 0 Or IsNull(dw_2.Object.prod_codigo[1]) Then
	lb_Estado	=	False
End If
	
If ls_Columna <> "clie_codigo" And &
	(dw_2.Object.clie_codigo[1]) = 0 Or IsNull(dw_2.Object.clie_codigo[1])  Then
	lb_Estado	=	False
End If

pb_grabar.Enabled			=	lb_Estado
pb_ins_det.Enabled			=	lb_Estado
pb_eli_det.Enabled			=	lb_Estado

If lb_Estado Then
	dw_3.Retrieve(dw_2.Object.clie_codigo[1], dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1], dw_2.Object.prod_codigo[1], dw_2.Object.plde_codigo[1])
Else
	dw_3.Reset()
End If
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			Commit;				
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
			ELSE
				lb_Retorno	=	True
	
				dw_2.ResetUpdate()
				dw_1.ResetUpdate()
			END IF
		ELSE
	  		F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
 		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public subroutine wf_child ();dw_2.getChild('vari_codigo', idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
If idwc_variedades.Retrieve(iuo_Especies.Codigo) = 0 Then idwc_variedades.InsertRow(0)

dw_2.getChild('prod_codigo', idwc_Productores)
idwc_Productores.SetTransObject(sqlca)
If idwc_Productores.Retrieve(-1) = 0 Then idwc_Productores.InsertRow(0)
end subroutine

public function boolean wf_planilla (long planilla);Long    ll_existe
Integer li_especie

ii_sw = 0

Select		coca_numero, espe_codigo
	Into		:ll_existe, :li_especie
	From		dbo.ctlcalcondicioncalidadenca
	Where	clie_codigo = :gi_codexport
	And 		coca_numero = :Planilla
	And		plde_codigo = :iuo_Planta.Codigo
	And		espe_codigo = :iuo_Especies.Codigo
	Using 		Sqlca;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura tabla ctlcalreclamosenca  ")
	Return False
ElseIf sqlca.sqlcode	=	0 Then

	If li_especie = dw_2.Object.espe_codigo[1] Then
		istr_busq.argum[1] = String(Planilla)
		ii_sw = 0
	Else
		ii_sw = 1
	End If
	
	Return True
Else
	Return False
End If
end function

public function long wf_correlativo (long planta);Long    ll_existe

Select		IsNull(Max(coca_numero), 0) + 1 
	Into		:ll_existe
	From		dbo.ctlcalcondicioncalidadenca
	Where	clie_codigo = :gi_codexport
	And		plde_codigo = :planta
	Using 		Sqlca;

If Sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura tabla ctlcalreclamosenca  ")
	Return -1
Else
	Return ll_existe
End If
end function

public subroutine wf_elimina ();Long 	ll_Fila, ll_Busca
String	ls_Busca

For ll_Fila = 1 To dw_1.RowCount()
	ls_Busca = "plde_codigo = " + String(dw_1.Object.plde_codigo[ll_Fila]) + "AND  plde_codpak = " + String(dw_1.Object.coca_packin[ll_Fila]) + &
					"AND emba_codigo = '"+ dw_1.Object.emba_codigo[ll_Fila]  + "'" +   "AND  String(cclo_fecemb) = '" +  String(dw_1.Object.coca_fecemb[ll_Fila]) + "'" + &
					"AND vaca_calibr ='"  + dw_1.Object.vaca_calibr[ll_Fila] + "'"
					
	ll_Busca = dw_3.Find(ls_Busca, 1, dw_3.RowCount())
	
	If ll_Busca > 0 Then dw_3.DeleteRow(ll_Busca)
Next
end subroutine

on w_maed_ctlcalcategoriacondicion.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.st_nota=create st_nota
this.ddlb_nota=create ddlb_nota
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.st_nota
this.Control[iCurrent+3]=this.ddlb_nota
end on

on w_maed_ctlcalcategoriacondicion.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.st_nota)
destroy(this.ddlb_nota)
end on

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1] =	String(iuo_Especies.Codigo)
istr_busq.argum[2] =	String(gi_CodExport)
istr_busq.argum[3] =  ""

OpenWithParm(w_busc_ctlcalcategoriascondicion, istr_busq)

istr_busq = Message.PowerObjectParm

If UpperBound(istr_busq.argum) > 2 Then
	If istr_busq.argum[3] <> "" Then
		iuo_Especies.Codigo		= Integer(istr_Busq.Argum[1])
		istr_mant.Argumento[1]	= istr_Busq.Argum[3]
		
		Habilitaingreso('espe_codigo')
		This.PostEvent("ue_recuperadatos")
	Else
		pb_buscar.SetFocus()
		HabilitaEncab(True)
	End If
End If
		
		
end event

event ue_recuperadatos;Long 		ll_fila_e, Respuesta

dw_2.SetTransObject(Sqlca)

Do	
	ll_fila_e	= dw_2.Retrieve(gi_codexport, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]), iuo_Planta.Codigo)

	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		If ll_fila_e > 0 Then
			Do
				dw_1.Retrieve(gi_codexport, iuo_Especies.Codigo, Long(istr_mant.Argumento[1]), iuo_Planta.Codigo)
				iuo_Especies.Codigo		=	dw_2.Object.espe_codigo[1]
				iuo_Variedades.Variedad	=	dw_2.Object.vari_codigo[1]
				iuo_Planta.Codigo			=	dw_2.Object.plde_codigo[1]
				istr_Mant.Argumento[2]	=	String(dw_2.Object.coca_fechas[1], 'dd/mm/yyyy')
				wf_Child()
				HabilitaIngreso('espe_codigo')
				wf_Elimina()
			Loop While Respuesta = 1
			
			HabilitaEncab(False)
			pb_imprimir.Enabled	= True
			pb_grabar.Enabled	= True
			pb_eliminar.Enabled	= True
		End If
		If Respuesta = 2 Then Close(This)
	End If
Loop While Respuesta = 1

If Respuesta = 2 Then Close(This)
end event

event ue_nuevo;call super::ue_nuevo;dw_2.SetFocus()
dw_2.SetColumn("coca_numero")
dw_3.Reset()

dw_2.Object.espe_codigo[1]	=	iuo_Especies.Codigo
dw_2.Object.clie_codigo[1]		=	gi_CodExport
dw_2.Object.plde_codigo[1]		=	iuo_Planta.Codigo

HabilitaEncab(True)

ddlb_nota.SelectItem(0)
SetNull(il_Nota)
ddlb_nota.BackColor = RGB(255,255,255)

end event

event open;call super::open;iuo_variedades	=	Create uo_variedades 
iuo_productor	=	Create uo_productores
iuo_especies	= 	Create uo_especie
iuo_Cliente		=	Create uo_clientesprod
iuo_Planta		=	Create uo_Plantadesp

If Not iuo_Especies.Existe(Integer(Message.StringParm), True, Sqlca) Then Return
if Not iuo_Planta.Existe(gi_CodPlanta, True, Sqlca) Then Return
This.Title		= "Categorias de Condicion: " + iuo_especies.Nombre
dw_1.Title	= "Detalle Categorias de Condicion: [" + iuo_especies.Nombre + "]"
istr_Mant.Argumento[2] = String(Today(), 'dd/mm/yyyy')

dw_3.SetTransObject(Sqlca)

wf_child()
end event

event ue_imprimir;Long	fila, ll_Fila
Date	ld_Fecha

SetPointer(HourGlass!)

istr_info.titulo	=	"INFORME DE CATEGORIAS DE CONDICION"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

ld_Fecha = dw_2.Object.coca_fechas[1]

vinf.dw_1.DataObject	=	"dw_info_categoriacondicion"
vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(gi_codexport, iuo_Especies.Codigo, dw_2.Object.coca_numero[1], ld_Fecha, ld_Fecha, -1, -1)

If fila	=	-1 Then
	MessageBox("Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + &
				sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox("No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
End If

SetPointer(Arrow!)
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount()  > 0	THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF

end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila, ll_Numero
Integer	li_cont
String		ls_colu[], ls_mensaje

If dw_2.RowCount() > 0 Then
	dw_2.Object.clie_codigo[1] = gi_CodExport

	If Isnull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nCódigo Variedad."
		ls_colu[li_cont]	= "vari_codigo"
	End If
	
	If Isnull(dw_2.Object.prod_codigo[1]) OR dw_2.Object.prod_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nCódigo Productor."
		ls_colu[li_cont]	= "prod_codigo"
	End If
	
	If Isnull(dw_2.Object.coca_fechas[1]) Then
		li_cont ++
		ls_mensaje 		= ls_mensaje + "~nFecha de Movimiento."
		ls_colu[li_cont]	= "coca_fechas"
	End If
End If

If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_2.SetColumn(ls_colu[1])
		dw_2.SetFocus()
		HabilitaEncab(True)
		Message.DoubleParm = -1
		Return
Else
	dw_2.Object.coca_fecdig[1] 	=	Today()
	dw_2.Object.coca_hordig[1] 	= 	Now()
	dw_2.Object.coca_usuari[1] 	=	gstr_us.Nombre
	If IsNull(dw_2.Object.coca_numero[1]) Then dw_2.Object.coca_numero[1]	=	wf_Correlativo(iuo_Planta.Codigo)
	
	For ll_Fila = 1 To dw_1.RowCount()
		dw_1.Object.clie_codigo[ll_fila]		= dw_2.Object.clie_codigo[1]
		dw_1.Object.espe_codigo[ll_fila]	= dw_2.Object.espe_codigo[1]
		dw_1.Object.coca_numero[ll_fila]	= dw_2.Object.coca_numero[1]
		dw_1.Object.vari_codigo[ll_fila]	= dw_2.Object.vari_codigo[1]
		//dw_1.Object.prod_codigo[ll_fila]	= dw_2.Object.prod_codigo[1]
		dw_1.Object.coca_fecins[ll_fila]	= dw_2.Object.coca_fechas[1]
	Next
End If
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0,&
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

If dw_2.width > il_AnchoDw_1 Then
	maximo		=	dw_2.width
Else
	dw_1.width	=	((This.WorkSpaceWidth() - 400) / 2 ) +570 //200
	dw_3.width	= 	dw_1.Width - 1170 //400
	maximo		=	dw_1.Width
End If

dw_2.x					= -537 + Round((maximo * 2 - dw_2.width) / 2, 0)
dw_2.y					= 37
//dw_2.width				= dw_2.width + 82
dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height
dw_1.Height			= This.WorkSpaceHeight() - dw_1.y - 41

dw_3.x					= dw_1.x + dw_1.Width + 50
dw_3.y					= 64 + dw_2.Height
dw_3.Height			= This.WorkSpaceHeight() - dw_1.y - 41

ddlb_nota.x				=	((dw_3.x + dw_3.Width) - ddlb_nota.Width) 
st_nota.x				=	ddlb_nota.x - st_nota.Width

li_posic_x				= This.WorkSpaceWidth() - 250
li_posic_y				= 88

If pb_buscar.Visible Then
	pb_buscar.x			= li_posic_x
	pb_buscar.y			= li_posic_y
	pb_buscar.width		= li_Ancho
	pb_buscar.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_nuevo.Visible Then
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width		= li_Ancho
	pb_nuevo.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If	pb_eliminar.Visible Then
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= li_Ancho
	pb_eliminar.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_grabar.Visible Then
	pb_grabar.x			= li_posic_x
	pb_grabar.y			= li_posic_y
	pb_grabar.width		= li_Ancho
	pb_grabar.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_imprimir.Visible Then
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= li_Ancho
	pb_imprimir.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_salir.Visible Then
	pb_salir.x			= li_posic_x
	pb_salir.y			= li_posic_y
	pb_salir.width		= li_Ancho
	pb_salir.height		= li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

pb_eli_det.x				=	li_posic_x
pb_eli_det.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_eli_det.width		=	li_Ancho
pb_eli_det.height		=	li_Alto

pb_ins_det.x			=	li_posic_x
pb_ins_det.y			=	pb_eli_det.y - li_Siguiente - 10
pb_ins_det.width		=	li_Ancho
pb_ins_det.height		=	li_Alto
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcalcategoriacondicion
integer x = 59
integer y = 660
integer width = 3072
integer height = 1228
string title = ""
string dataobject = "dw_mues_ctlcalcategoriacondiciondeta"
boolean livescroll = false
end type

event dw_1::doubleclicked;//
end event

event dw_1::sqlpreview;//
end event

event dw_1::clicked;call super::clicked;/*
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
	
	This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 255, 255)))
	
	This.Sort()
End If
end event

event dw_1::buttonclicked;call super::buttonclicked;String		ls_Boton
Str_mant	lstr_mant

ls_Boton = dwo.Name

Choose Case ls_Boton
	Case 'b_observa'
		lstr_Mant.dw = dw_1
	
		OpenWithParm(w_mant_deta_condicioncalidad_obs, lstr_mant) 
		
		
End Choose 
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcalcategoriacondicion
integer x = 110
integer y = 32
integer width = 1975
integer height = 584
integer taborder = 10
string dataobject = "dw_mant_categoriacondicionenca"
boolean controlmenu = true
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)
ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "coca_numero"
		If wf_Planilla(Long(data)) Then
			istr_mant.Argumento[1]	=	Data
			If ii_sw = 0 Then
				Parent.PostEvent("ue_recuperadatos")
			Else
				Messagebox("Atención","Planilla Digitada existe para otra Especie, no puede ingresar", Exclamation!, Ok!)
				dw_2.Object.coca_numero.Protect	=	0
				dw_2.SetItem(Row,ls_Columna,Long(ls_Nula))
				Return 1
			End If
			dw_2.SetColumn(ls_Columna)
			dw_2.SetFocus()
		Else
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		End If
		
	Case "espe_codigo"
		If Not iuo_Especies.Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		Else
			wf_Child()
		End If	

	Case "plde_codigo"
		If Not iuo_Planta.Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		End If	
		
	Case "clie_codigo"
		If Not iuo_Cliente.Existe(Integer(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		End If	
		
	Case "vari_codigo"
		If Not iuo_Variedades.Existe(iuo_Especies.Codigo, Integer(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		End If	
		
	Case "prod_codigo"
		If Not iuo_Productor.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
			Return 1
		else 
			This.SetItem(1,'ticket', Long(1))
		End If	
	
	Case "ticket"
			This.SetItem(1,'prod_codigo', Long(data))
			
	Case 'coca_fechas'
		If date(data) > date(today()) Then
			Messagebox('Atención','La Fecha de Inspección no debe ser superior a la Fecha Actual')
			This.SetItem(1, ls_Columna, Date(today()))
			Return 1
		Else
			istr_Mant.Argumento[2] = Data
		End If
End Choose

Habilitaingreso(ls_Columna) 
end event

event dw_2::sqlpreview;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcalcategoriacondicion
integer x = 5157
integer y = 332
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;dw_2.Object.vari_codigo[1]		= iuo_Variedades.Variedad
dw_2.Object.coca_fechas[1]	= Date(istr_Mant.Argumento[2])
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcalcategoriacondicion
integer x = 5170
integer y = 508
integer taborder = 60
boolean enabled = true
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcalcategoriacondicion
integer x = 5166
integer y = 696
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcalcategoriacondicion
integer x = 5166
integer y = 868
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcalcategoriacondicion
integer x = 5166
integer y = 1052
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcalcategoriacondicion
integer x = 5175
integer y = 1344
integer taborder = 0
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_ins_det::clicked;Long 	ll_Fila, ll_New, ll_Busca
Integer li_resp
String	ls_Busca

IF IsNull(il_Nota) or il_Nota = 0 THEN	li_resp = MessageBox("Atención","Falta Asignar Nota de Segregación a Estos Lotes,  Continua?",Exclamation!,YesNo!)

IF li_resp = 2 THEN RETURN

For ll_Fila = 1 To dw_3.RowCount()
	If dw_3.IsSelected(ll_Fila) Then
		
		ls_Busca = "plde_codigo = " + String(dw_3.Object.plde_codigo[ll_Fila]) + "AND  coca_packin = " + String(dw_3.Object.plde_codpak[ll_Fila]) +  "AND  prod_codigo = " + String(dw_3.Object.prod_codigo[ll_Fila]) +&
						"AND emba_codigo = '"+ dw_3.Object.emba_codigo[ll_Fila]  + "'" +   "AND  String(coca_fecemb) = '" +  String(dw_3.Object.cclo_fecemb[ll_Fila]) + "'" + &
						"AND vaca_calibr ='"  + dw_3.Object.vaca_calibr[ll_Fila] + "'"
						
		ll_Busca = dw_1.Find(ls_Busca, 1, dw_1.RowCount())
		
		If ll_Busca = 0 Then
			ll_New	= dw_1.InsertRow(0)
			
			dw_1.Object.plde_codigo[ll_New]		=	dw_3.Object.plde_codigo[ll_Fila]
			dw_1.Object.coca_packin[ll_New]		=	dw_3.Object.plde_codpak[ll_Fila]
			dw_1.Object.prod_codigo[ll_New]		=	dw_3.Object.prod_codigo[ll_Fila]
			dw_1.Object.emba_codigo[ll_New]	=	dw_3.Object.emba_codigo[ll_Fila]
			dw_1.Object.coca_fecemb[ll_New]	=	dw_3.Object.cclo_fecemb[ll_Fila]
			dw_1.Object.coca_cancaj[ll_New]		=	dw_3.Object.cajas[ll_Fila]	
			dw_1.Object.vaca_calibr[ll_New]		=	dw_3.Object.vaca_calibr[ll_Fila]	
			dw_1.Object.coca_fecins[ll_New]		=	dw_2.Object.coca_fechas[1]
			dw_1.Object.coca_catcon[ll_New]		=	il_Nota		
			dw_1.Object.etiq_codigo[ll_New]		=	dw_3.Object.etiq_codigo[ll_Fila]
		Else
			MessageBox('Atencion', 'Registro Nro.: ' + String(ll_Fila) + ', ya fue marcado.', StopSign!, Ok!)
		End If
	End If	
Next

ddlb_nota.SelectItem(0)
SetNull(il_Nota)
ddlb_nota.BackColor = RGB(255,255,255)

wf_Elimina()
end event

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcalcategoriacondicion
integer x = 5175
integer y = 1516
integer taborder = 0
end type

event pb_eli_det::clicked;call super::clicked;If dw_1.rowcount() < 1 Then Return

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

If Message.DoubleParm = -1 Then Return

If MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 Then
	If dw_1.DeleteRow(0) = 1 Then
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	Else
		ib_borrar = False
		MessageBox(Parent.Title,"No se puede borrar actual registro.")
	End If

 If dw_1.RowCount() = 0 Then
		pb_eliminar.Enabled = False
	Else
		il_fila = dw_1.GetRow()
		dw_3.Retrieve(dw_2.Object.clie_codigo[1], dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1], dw_2.Object.prod_codigo[1], dw_2.Object.plde_codigo[1])
		wf_Elimina()
	End If
End If
end event

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcalcategoriacondicion
integer x = 5157
integer y = 152
integer taborder = 30
boolean default = true
end type

type dw_3 from uo_dw within w_maed_ctlcalcategoriacondicion
integer x = 3141
integer y = 664
integer width = 1952
integer height = 1228
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Consulta Lotes"
string dataobject = "dw_genera_calidadcondicion"
boolean hscrollbar = true
end type

event clicked;call super::clicked;String  ls_Tecla, ls_old_sort, ls_column, ls_color_old
Char		lc_sort

IF KeyDown(KeyShift!) THEN
	ls_tecla = "Shift"
ELSEIF KeyDown(KeyControl!) THEN
	ls_tecla = "Control"
END IF

If Row > 0 Then
	il_fila = Row
	F_Selecciona(This, ls_tecla, Row)
End If

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
	
	This.Modify(dwo.Name + ".Color = " + String(Rgb(0, 255, 255)))
	
	This.Sort()
End If
end event

type st_nota from statictext within w_maed_ctlcalcategoriacondicion
integer x = 4635
integer y = 572
integer width = 165
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 30586022
string text = "Nota"
boolean focusrectangle = false
end type

type ddlb_nota from dropdownpicturelistbox within w_maed_ctlcalcategoriacondicion
integer x = 4814
integer y = 560
integer width = 242
integer height = 400
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string item[] = {"1","2","3","4","5"}
borderstyle borderstyle = stylelowered!
integer itempictureindex[] = {1,2,3,4,5}
long picturemaskcolor = 536870912
end type

event selectionchanged;Choose Case Index
	Case	1 
		il_Nota = 1
		This.BackColor = RGB(0,255,0) 
	Case	2
		il_Nota = 2
		This.BackColor = RGB(255,255,0)
	Case	3
		il_Nota = 3
		This.BackColor = RGB(255,128,0)
	Case	4
		il_Nota = 4
		This.BackColor = RGB(255,0,0)		
	Case	5
		il_Nota = 5
		This.BackColor = RGB(255,0,0)		
	Case Else
		SetNull(il_Nota)
		This.BackColor = RGB(255,255,255)		
End Choose
dw_3.setfocus()

end event

event getfocus;this.BackColor = RGB(255,255,255)		
end event

