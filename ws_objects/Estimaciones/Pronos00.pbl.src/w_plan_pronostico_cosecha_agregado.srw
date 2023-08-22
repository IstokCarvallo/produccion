$PBExportHeader$w_plan_pronostico_cosecha_agregado.srw
forward
global type w_plan_pronostico_cosecha_agregado from w_mant_directo
end type
type dw_2 from datawindow within w_plan_pronostico_cosecha_agregado
end type
type dw_3 from datawindow within w_plan_pronostico_cosecha_agregado
end type
type dw_5 from datawindow within w_plan_pronostico_cosecha_agregado
end type
type dw_4 from datawindow within w_plan_pronostico_cosecha_agregado
end type
end forward

global type w_plan_pronostico_cosecha_agregado from w_mant_directo
integer width = 4334
integer height = 2040
string title = "Ficha Pronóstico"
windowstate windowstate = maximized!
event ue_correo ( )
dw_2 dw_2
dw_3 dw_3
dw_5 dw_5
dw_4 dw_4
end type
global w_plan_pronostico_cosecha_agregado w_plan_pronostico_cosecha_agregado

type variables
uo_Especie 			iuo_Especie
uo_Variedades 		iuo_Variedad
uo_Productores 	iuo_Productor
uo_Prodpredio		iuo_Predio
uo_ProdCuarteles	iuo_Cuartel
uo_NroSemana		iuo_Semana

DataStore			ids_Calibre

DataWindowChild 	idwc_especie, idwc_variedad, idwc_predio, idwc_productor,idwc_Cuartel

String			is_Archivo,is_Directorio, is_col[]
Integer		il_cancol,ii_Semanas[],il_Col_tot_Semana,il_Col_tot_Calibre
end variables

forward prototypes
public subroutine habilitaenca (boolean ab_habilita)
public subroutine buscanombres (integer ssa)
protected function boolean wf_actualiza_db ()
public function boolean wf_traspasadistribucion ()
public function string wf_crea_datawindow_tabla ()
public function boolean wf_crea_datawindow ()
public function string wf_crea_datawindow_head ()
public subroutine wf_totalsemana (long fila)
public subroutine wf_totalcalibre (long fila)
public subroutine wf_recalcula (integer fila)
end prototypes

public subroutine habilitaenca (boolean ab_habilita);If ab_habilita Then
	dw_2.Object.dfce_semter.Protect 					=	0
	dw_2.Object.dfce_semter.BackGround.Color	=	RGB(255,255,255)
Else
	dw_2.Object.dfce_semter.Protect 					=	1
	dw_2.Object.dfce_semter.BackGround.Color	=	RGB(166,180,210)
End If
end subroutine

public subroutine buscanombres (integer ssa);
end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_check_required(0) THEN RETURN False
IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_2.Update(True, False) = 1 THEN 
	IF dw_3.Update(True, False) = 1 THEN 
		IF dw_4.Update(True,False) = 1 THEN
			IF dw_5.Update(True,False) = 1 THEN
				Commit;
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					lb_Retorno	=	False
				ELSE
					lb_Retorno	=	True
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					dw_3.ResetUpdate()
					dw_4.ResetUpdate()
					dw_5.ResetUpdate()
				END IF
			ELSE
				RollBack;
				IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
				lb_Retorno	=	False
			END IF
		ELSE
			RollBack;
			IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
		END IF
	ELSE
		RollBack;
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	END IF
ELSE
	RollBack;
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
END IF
	  
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean wf_traspasadistribucion ();Boolean	lb_Retorno = True

Long		ll_Identificador

ll_Identificador	= dw_2.Object.dfce_identi[1]

Declare Traspasa Procedure For dbo.Pron_DistribucionAgrupada
	@Identificador	=	:ll_Identificador
	Using SQLCA ;

Execute Traspasa;	

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLCA,"Problema en Ejecucion de Distribucion Agrupada" )
	lb_Retorno = False
End If

Close Traspasa;
Commit;

Return lb_Retorno
end function

public function string wf_crea_datawindow_tabla ();String ls_syntax_tabl,ls_semana,ls_calibre
Int	 li_inic_c, li_term_c
Int	 li_inic_s, li_term_s

li_term_c	=	ids_calibre.RowCount()
li_term_s	=	UpperBound(ii_semanas)

// Tabla y Columnas
ls_syntax_tabl = "table("
ls_syntax_tabl = ls_syntax_tabl + "column=(type=number updatewhereclause=yes name=identificador dbname="+char(34)+"Identificador"+char(34)+" )"
ls_syntax_tabl = ls_syntax_tabl + " column=(type=number updatewhereclause=yes name=cod_condicion dbname="+char(34)+"Cod_Condicion"+char(34)+" )"
ls_syntax_tabl = ls_syntax_tabl + " column=(type=char(30) updatewhereclause=yes name=condicion dbname="+char(34)+"Condicion"+char(34)+" )"
ls_syntax_tabl = ls_syntax_tabl + " column=(type=number updatewhereclause=yes name=tipo_registro dbname="+char(34)+"Tipo_Registro"+char(34)+" values="+char(34)+"Porcentaje	1/Cajas	2/"+char(34)+" )"
ls_syntax_tabl = ls_syntax_tabl + " column=(type=decimal(2) updatewhereclause=yes name=valor_condicion dbname="+char(34)+"Valor_Condicion"+char(34)+" )"

FOR li_inic_s = 1 to li_term_s
	ls_semana   = 'Valor_Sem'+String(ii_semanas[li_inic_s],'00')
	ls_syntax_tabl = ls_syntax_tabl + " column=(type=decimal(2) updatewhereclause=yes name="+ls_semana+" dbname="+char(34)+ls_Semana+char(34)+" )"
	il_cancol ++
	is_col[il_cancol] = ls_Semana
NEXT
ls_syntax_tabl = ls_syntax_tabl + " column=(type=decimal(2) updatewhereclause=yes name=total_semana dbname="+char(34)+"Total_Semana"+char(34)+" )"

FOR li_inic_c = 1 to li_term_c
	ls_calibre	= "Valor_Cal"+ids_calibre.object.vaca_calibr[li_inic_c]
	ls_syntax_tabl = ls_syntax_tabl + " column=(type=decimal(2) updatewhereclause=yes name="+ls_calibre+" dbname="+char(34)+ls_Calibre+char(34)+" )"
	il_cancol ++
	is_col[il_cancol] = ls_calibre
NEXT
ls_syntax_tabl = ls_syntax_tabl + " column=(type=decimal(2) updatewhereclause=yes name=total_calibre dbname="+char(34)+"Total_Calibre"+char(34)+" )"
ls_syntax_tabl = ls_syntax_tabl + " )"

return ls_syntax_tabl
end function

public function boolean wf_crea_datawindow ();String ls_syntax_gene
String ls_syntax_tabl
String ls_syntax_head
String ls_syntax_colu
String ls_errors
String ls_calibre, ls_semana
Int	 li_inic_s, li_term_s, li_id, il_tab, li_insert
Int	 li_inic_c, li_term_c
Long   ll_head_w, ll_head_x, ll_head_y, ll_head_h

li_term_c = ids_calibre.RowCount()
li_term_s = UpperBound(ii_Semanas)
il_cancol = 0

// General (Encabezado)
ls_syntax_gene = "release 17;"&
+ "datawindow(units=0 timer_interval=0 color=1073741824 brushmode=0 transparency=0 gradient.angle=0 gradient.color=8421504 gradient.focus=0 gradient.repetition.count=0 gradient.repetition.length=100 gradient.repetition.mode=0 gradient.scale=100 gradient.spread=100 gradient.transparency=0 picture.blur=0 picture.clip.bottom=0 picture.clip.left=0 picture.clip.right=0 picture.clip.top=0 picture.mode=0 picture.scale.x=100 picture.scale.y=100 picture.transparency=0 processing=0 HTMLDW=no print.printername="+char(34)+""+char(34)+" print.documentname="+char(34)+""+char(34)+" print.orientation = 0 print.margin.left = 110 print.margin.right = 110 print.margin.top = 96 print.margin.bottom = 96 print.paper.source = 0 print.paper.size = 0 print.canusedefaultprinter=yes print.prompt=no print.buttons=no print.preview.buttons=no print.cliptext=no print.overrideprintjob=no print.collate=yes print.background=no print.preview.background=no print.preview.outline=yes hidegrayline=no showbackcoloronxp=no picture.file="+char(34)+""+char(34)+" )" &
+ "header(height=100 color="+char(34)+"536870912"+char(34)+" transparency="+char(34)+"0"+char(34)+" gradient.color="+char(34)+"8421504"+char(34)+" gradient.transparency="+char(34)+"0"+char(34)+" gradient.angle="+char(34)+"0"+char(34)+" brushmode="+char(34)+"0"+char(34)+" gradient.repetition.mode="+char(34)+"0"+char(34)+" gradient.repetition.count="+char(34)+"0"+char(34)+" gradient.repetition.length="+char(34)+"100"+char(34)+" gradient.focus="+char(34)+"0"+char(34)+" gradient.scale="+char(34)+"100"+char(34)+" gradient.spread="+char(34)+"100"+char(34)+" )" &
+ "summary(height=64 color="+char(34)+"536870912"+char(34)+" transparency="+char(34)+"0"+char(34)+" gradient.color="+char(34)+"8421504"+char(34)+" gradient.transparency="+char(34)+"0"+char(34)+" gradient.angle="+char(34)+"0"+char(34)+" brushmode="+char(34)+"0"+char(34)+" gradient.repetition.mode="+char(34)+"0"+char(34)+" gradient.repetition.count="+char(34)+"0"+char(34)+" gradient.repetition.length="+char(34)+"100"+char(34)+" gradient.focus="+char(34)+"0"+char(34)+" gradient.scale="+char(34)+"100"+char(34)+" gradient.spread="+char(34)+"100"+char(34)+" )" &
+ "footer(height=340 color="+char(34)+"536870912"+char(34)+" transparency="+char(34)+"0"+char(34)+" gradient.color="+char(34)+"8421504"+char(34)+" gradient.transparency="+char(34)+"0"+char(34)+" gradient.angle="+char(34)+"0"+char(34)+" brushmode="+char(34)+"0"+char(34)+" gradient.repetition.mode="+char(34)+"0"+char(34)+" gradient.repetition.count="+char(34)+"0"+char(34)+" gradient.repetition.length="+char(34)+"100"+char(34)+" gradient.focus="+char(34)+"0"+char(34)+" gradient.scale="+char(34)+"100"+char(34)+" gradient.spread="+char(34)+"100"+char(34)+" )" &
+ "detail(height=60 color="+char(34)+"536870912"+char(34)+" transparency="+char(34)+"0"+char(34)+" gradient.color="+char(34)+"8421504"+char(34)+" gradient.transparency="+char(34)+"0"+char(34)+" gradient.angle="+char(34)+"0"+char(34)+" brushmode="+char(34)+"0"+char(34)+" gradient.repetition.mode="+char(34)+"0"+char(34)+" gradient.repetition.count="+char(34)+"0"+char(34)+" gradient.repetition.length="+char(34)+"100"+char(34)+" gradient.focus="+char(34)+"0"+char(34)+" gradient.scale="+char(34)+"100"+char(34)+" gradient.spread="+char(34)+"100"+char(34)+" )" 

ls_syntax_tabl = wf_crea_Datawindow_Tabla()
//ls_syntax_tabl =  ls_syntax_tabl+" sort="+char(34)+"identificador A cod_condicion A tipo_registro A "+char(34)+" ) "

// Header
ls_syntax_head = ls_syntax_head + wf_crea_datawindow_head()

// Detail y Summary
//Desde la columna 3 que se despliega.

ls_syntax_colu = ls_syntax_colu + "column(band=detail id=3 alignment="+char(34)+"0"+char(34)+" tabsequence=0 border="+char(34)+"2"+char(34)+" color="+char(34)+"33554432"+char(34)+" x="+char(34)+"123"+char(34)+" y="+char(34)+"4"+char(34)+" height="+char(34)+"48"+char(34)+" width="+char(34)+"288"+char(34)+" format="+char(34)+"[general]"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=condicion visible="+char(34)+"1"+char(34)+" edit.limit=0 edit.case=any edit.autoselect=yes edit.autohscroll=yes  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"400"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"1"+char(34)+" background.color="+char(34)+"536870912"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
ls_syntax_colu = ls_syntax_colu + "column(band=detail id=4 alignment="+char(34)+"0"+char(34)+" tabsequence=0 border="+char(34)+"2"+char(34)+" color="+char(34)+"33554432"+char(34)+" x="+char(34)+"430"+char(34)+" y="+char(34)+"4"+char(34)+" height="+char(34)+"48"+char(34)+" width="+char(34)+"247"+char(34)+" format="+char(34)+"[general]"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=tipo_registro visible="+char(34)+"1"+char(34)+" ddlb.limit=0 ddlb.allowedit=no ddlb.case=any  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"400"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"0"+char(34)+" background.color="+char(34)+"536870912~tif(Tipo_Registro = 1,rgb(166,180,210),RGB(255,255,255))"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
ls_syntax_colu = ls_syntax_colu + "column(band=detail id=5 alignment="+char(34)+"1"+char(34)+" tabsequence=10 border="+char(34)+"2"+char(34)+" color="+char(34)+"33554432"+char(34)+" x="+char(34)+"695"+char(34)+" y="+char(34)+"4"+char(34)+" height="+char(34)+"48"+char(34)+" width="+char(34)+"233"+char(34)+" format="+char(34)+"[general]~tif(tipo_registro = 1,'#.##0.00','#,##0')"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=valor_condicion visible="+char(34)+"1"+char(34)+" edit.limit=0 edit.case=any edit.autoselect=yes edit.autohscroll=yes  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"400"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"0"+char(34)+" background.color="+char(34)+"536870912~tif(Tipo_Registro = 1,rgb(166,180,210),RGB(255,255,255))"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"

ls_syntax_colu = ls_syntax_colu + "text(band=summary alignment="+char(34)+"1"+char(34)+" text="+char(34)+"Totales Generales Distribución"+char(34)+" border="+char(34)+"6"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+"23"+char(34)+" y="+char(34)+"8"+char(34)+" height="+char(34)+"44"+char(34)+" width="+char(34)+"654"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=t_1 visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"8388608"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
ls_syntax_colu = ls_syntax_colu + "compute(band=summary alignment="+char(34)+"1"+char(34)+" expression="+char(34)+"sum(( tipo_registro - 1) * valor_condicion for all)"+char(34)+"enabled="+char(34)+"0"+char(34)+" border="+char(34)+"0"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+"695"+char(34)+" y="+char(34)+"4"+char(34)+" height="+char(34)+"48"+char(34)+" width="+char(34)+"233"+char(34)+" format="+char(34)+"#,##0"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=compute_14 visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"134217859"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"

li_id			=	5
il_tab			=	10
ll_head_x	= 946
ll_head_y	= 4
ll_head_h	= 44
ll_head_w	= 178

FOR li_inic_s = 1 TO li_term_s
	ls_semana   = "valor_sem"+String(ii_semanas[li_inic_s],'00')
	li_id ++
	il_tab = il_tab + 10
	ls_syntax_colu = ls_syntax_colu + "column(band=detail id="+String(li_id)+" alignment="+char(34)+"1"+char(34)+" tabsequence="+String(il_tab)+" border="+char(34)+"2"+char(34)+" color="+char(34)+"33554432"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+"  format="+char(34)+"[general]~tif(tipo_registro = 1,'#.##0.00','#,##0')"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name="+ls_semana+" visible="+char(34)+"1"+char(34)+" edit.limit=0 edit.case=any edit.autoselect=yes edit.autohscroll=yes  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"400"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"0"+char(34)+" background.color="+char(34)+"536870912~tif(Tipo_Registro = 1,rgb(166,180,210),RGB(255,255,255))"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
	ls_syntax_colu = ls_syntax_colu + "compute(band=summary alignment="+char(34)+"1"+char(34)+" expression="+char(34)+"sum(( tipo_registro - 1) * "+ls_semana+" for all)"+char(34)+"enabled="+char(34)+"0"+char(34)+" border="+char(34)+"0"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+"  format="+char(34)+"#,##0"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=tot_"+ls_semana+" visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"134217859"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"

	ll_head_x = ll_head_x + ll_head_w + 20
NEXT

li_id ++ 
il_Col_tot_Semana = li_id
ls_syntax_colu = ls_syntax_colu + "column(band=detail id="+String(li_id)+" alignment="+char(34)+"1"+char(34)+" tabsequence=0 border="+char(34)+"2"+char(34)+" color="+char(34)+"33554432"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+" format="+char(34)+"[general]~tif(tipo_registro = 1,'#.##0.00','#,##0')"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=total_semana visible="+char(34)+"1"+char(34)+" edit.limit=0 edit.case=any edit.autoselect=yes edit.autohscroll=yes  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"400"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"16776960~tif(tipo_registro = 2,if (valor_condicion = Total_Semana,RGB(0,255,255),RGB(255,0,0)),if( Total_Semana = 100,RGB(0,255,255),RGB(255,0,0)))"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
ll_head_x = ll_head_x + ll_head_w + 20

FOR li_inic_c = 1 TO li_term_c
	ls_calibre   = 'Valor_Cal'+ids_calibre.object.vaca_calibr[li_inic_c]
	li_id ++
	il_tab = il_tab + 10
	ls_syntax_colu = ls_syntax_colu + " column(band=detail id="+String(li_id)+" alignment="+char(34)+"1"+char(34)+" tabsequence="+String(il_tab)+" border="+char(34)+"2"+char(34)+" color="+char(34)+"33554432"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+"  format="+char(34)+"[general]~tif(tipo_registro = 1,'#.##0.00','#,##0')"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name="+ls_calibre+" visible="+char(34)+"1"+char(34)+" edit.limit=0 edit.case=any edit.autoselect=yes edit.autohscroll=yes  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"400"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"0"+char(34)+" background.color="+char(34)+"536870912~tif(Tipo_Registro = 1,rgb(166,180,210),RGB(255,255,255))"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
	ls_syntax_colu = ls_syntax_colu + " compute(band=summary alignment="+char(34)+"1"+char(34)+" expression="+char(34)+"sum(( tipo_registro - 1) * "+ls_calibre+" for all)"+char(34)+"enabled="+char(34)+"0"+char(34)+" border="+char(34)+"0"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+"  format="+char(34)+"#,##0"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=tot_"+ls_calibre+" visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"134217859"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"

	ll_head_x = ll_head_x + ll_head_w + 20
NEXT

li_id ++
il_Col_tot_Calibre = li_id
ls_syntax_colu = ls_syntax_colu + " column(band=detail id="+String(li_id)+" alignment="+char(34)+"1"+char(34)+" tabsequence=0 border="+char(34)+"2"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+" format="+char(34)+"[general]~tif(tipo_registro = 1,'#.##0.00','#,##0')"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=total_calibre visible="+char(34)+"1"+char(34)+" edit.limit=0 edit.case=any edit.autoselect=yes edit.autohscroll=yes  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"400"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"134217741~tif(tipo_registro = 2,if (valor_condicion = Total_Calibre,134217741,RGB(255,0,0)),if (total_calibre = 100,134217741,RGB(255,0,0)))"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"8421504"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"0"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"

ls_syntax_colu = ls_syntax_colu + " sparse(names="+char(34)+"condicion	tipo_registro"+char(34)+")htmltable(border="+char(34)+"1"+char(34)+" ) "
/*
ls_syntax_colu = ls_syntax_colu + " htmlgen(clientevents="+char(34)+"1"+char(34)+" clientvalidation="+char(34)+"1"+char(34)+" clientcomputedfields="+char(34)+"1"+char(34)+" clientformatting="+char(34)+"0"+char(34)+" clientscriptable="+char(34)+"0"+char(34)+" generatejavascript="+char(34)+"1"+char(34)+" encodeselflinkargs="+char(34)+"1"+char(34)+" netscapelayers="+char(34)+"0"+char(34)+" pagingmethod=0 generatedddwframes="+char(34)+"1"+char(34)+" )"
ls_syntax_colu = ls_syntax_colu + " xhtmlgen() cssgen(sessionspecific="+char(34)+"0"+char(34)+" )"
ls_syntax_colu = ls_syntax_colu + " xmlgen(inline="+char(34)+"0"+char(34)+" )"
ls_syntax_colu = ls_syntax_colu + " xsltgen()"
ls_syntax_colu = ls_syntax_colu + " jsgen()"
ls_syntax_colu = ls_syntax_colu + " export.xml(headgroups="+char(34)+"1"+char(34)+" includewhitespace="+char(34)+"0"+char(34)+" metadatatype=0 savemetadata=0 )"
ls_syntax_colu = ls_syntax_colu + " import.xml()"
ls_syntax_colu = ls_syntax_colu + " export.pdf(method=0 distill.custompostscript="+char(34)+"0"+char(34)+" xslfop.print="+char(34)+"0"+char(34)+" nativepdf.customsize=0 nativepdf.customorientation=0 nativepdf.pdfstandard=0 nativepdf.useprintspec=no )"
ls_syntax_colu = ls_syntax_colu + " export.xhtml()"

*/
// Creación de DataWindows Dinamica
dw_1.Create(ls_syntax_gene + ls_syntax_tabl + ls_syntax_head + ls_syntax_colu, ls_errors)
if ls_errors <> '' then
	MessageBox("Error...", "Se ha producido un error al momento de extraer información,. Consulte con Administrador...", StopSign!, Ok!)
	Return false
end if
dw_1.SetTransObject(sqlca)

return true
end function

public function string wf_crea_datawindow_head ();String ls_syntax_head, ls_calibre, ls_semana
Long ll_head_x = 946
Long ll_head_y = 4
Long ll_head_h = 44
Long ll_head_w = 178
Int	 li_inic_c=1, li_term_c, li_cal
Int	 li_inic_s=1, li_term_s, li_sem

li_term_c	=	ids_calibre.RowCount()
li_term_s	=	UpperBound(ii_semanas)

ls_syntax_head = " text(band=header alignment="+char(34)+"2"+char(34)+" text="+char(34)+"Condición"+char(34)+" border="+char(34)+"6"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+"123"+char(34)+" y="+char(34)+"4"+char(34)+" height="+char(34)+"44"+char(34)+" width="+char(34)+"288"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=condicion_t visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"8388608"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"16711680"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"5"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
ls_syntax_head = ls_syntax_head + " text(band=header alignment="+char(34)+"2"+char(34)+" text="+char(34)+"Tipo Reg."+char(34)+" border="+char(34)+"6"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+"430"+char(34)+" y="+char(34)+"4"+char(34)+" height="+char(34)+"44"+char(34)+" width="+char(34)+"247"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=tipo_registro_t visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"8388608"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"16711680"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"5"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
ls_syntax_head = ls_syntax_head + " text(band=header alignment="+char(34)+"2"+char(34)+" text="+char(34)+"Val.Cond."+char(34)+" border="+char(34)+"6"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+"695"+char(34)+" y="+char(34)+"4"+char(34)+" height="+char(34)+"44"+char(34)+" width="+char(34)+"233"+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=valor_condicion_t visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"8388608"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"16711680"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"5"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"

FOR li_sem = li_inic_s to li_term_s
	ls_semana = "Sem"+String(ii_Semanas[li_sem],'00')
	ls_syntax_head = ls_syntax_head + " text(band=header alignment="+char(34)+"2"+char(34)+" text="+char(34)+ls_Semana+char(34)+" border="+char(34)+"6"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=valor_sem"+String(li_sem,'00')+"_t visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"8388608"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"16711680"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"5"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
	ll_head_x = ll_head_x + ll_head_w + 18
NEXT

ls_syntax_head = ls_syntax_head + " text(band=header alignment="+char(34)+"2"+char(34)+" text="+char(34)+"Tt.Sem."+char(34)+" border="+char(34)+"6"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=total_semana_t visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"16776960"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"16711680"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"5"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
ll_head_x = ll_head_x + ll_head_w + 18

FOR li_cal = li_inic_c to li_term_c
	ls_calibre = ids_Calibre.object.vaca_calibr[li_cal]
	ls_syntax_head = ls_syntax_head + " text(band=header alignment="+char(34)+"2"+char(34)+" text="+char(34)+ls_calibre+char(34)+" border="+char(34)+"6"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=valor_cal"+ls_Calibre+"_t visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"8388608"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"16711680"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"5"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"
	ll_head_x = ll_head_x + ll_head_w + 18
NEXT

ls_syntax_head = ls_syntax_head + " text(band=header alignment="+char(34)+"2"+char(34)+" text="+char(34)+"Tt.Cal."+char(34)+" border="+char(34)+"6"+char(34)+" color="+char(34)+"16777215"+char(34)+" x="+char(34)+string(ll_head_x)+char(34)+" y="+char(34)+string(ll_head_y)+char(34)+" height="+char(34)+string(ll_head_h)+char(34)+" width="+char(34)+string(ll_head_w)+char(34)+" html.valueishtml="+char(34)+"0"+char(34)+"  name=total_calibre_t visible="+char(34)+"1"+char(34)+"  font.face="+char(34)+"Arial"+char(34)+" font.height="+char(34)+"-7"+char(34)+" font.weight="+char(34)+"700"+char(34)+"  font.family="+char(34)+"2"+char(34)+" font.pitch="+char(34)+"2"+char(34)+" font.charset="+char(34)+"0"+char(34)+" background.mode="+char(34)+"2"+char(34)+" background.color="+char(34)+"134217741"+char(34)+" background.transparency="+char(34)+"0"+char(34)+" background.gradient.color="+char(34)+"16711680"+char(34)+" background.gradient.transparency="+char(34)+"0"+char(34)+" background.gradient.angle="+char(34)+"0"+char(34)+" background.brushmode="+char(34)+"5"+char(34)+" background.gradient.repetition.mode="+char(34)+"0"+char(34)+" background.gradient.repetition.count="+char(34)+"0"+char(34)+" background.gradient.repetition.length="+char(34)+"100"+char(34)+" background.gradient.focus="+char(34)+"0"+char(34)+" background.gradient.scale="+char(34)+"100"+char(34)+" background.gradient.spread="+char(34)+"100"+char(34)+" tooltip.backcolor="+char(34)+"134217752"+char(34)+" tooltip.delay.initial="+char(34)+"0"+char(34)+" tooltip.delay.visible="+char(34)+"32000"+char(34)+" tooltip.enabled="+char(34)+"0"+char(34)+" tooltip.hasclosebutton="+char(34)+"0"+char(34)+" tooltip.icon="+char(34)+"0"+char(34)+" tooltip.isbubble="+char(34)+"0"+char(34)+" tooltip.maxwidth="+char(34)+"0"+char(34)+" tooltip.textcolor="+char(34)+"134217751"+char(34)+" tooltip.transparency="+char(34)+"0"+char(34)+" transparency="+char(34)+"0"+char(34)+" )"

return ls_syntax_head
end function

public subroutine wf_totalsemana (long fila);//Por defecto siempre viene la fila de los porcentajes
Integer	li_Columna
Decimal{2}	ld_Porcen, ld_TotalP
Long			ll_Cajas, ll_TotalC

dw_1.AcceptText()

FOR li_Columna = 6 to il_Col_tot_Semana - 1
	ld_Porcen	=	dw_1.GetItemDecimal(fila,li_Columna)
	IF Isnull(ld_Porcen) THEN 
		ld_Porcen = 0 
		dw_1.SetItem(fila,li_Columna,ld_Porcen)
	END IF
	ld_TotalP		=	ld_TotalP + ld_Porcen
	ll_Cajas		=	dw_1.GetItemDecimal(fila + 1,li_Columna)
	IF Isnull(ll_Cajas) THEN 
		ll_Cajas = 0
		dw_1.SetItem(fila + 1,li_Columna,ll_Cajas)
	END IF		
	ll_TotalC		=	ll_TotalC + ll_Cajas
NEXT

dw_1.SetItem(fila,il_Col_tot_Semana,ld_TotalP)
dw_1.SetItem(fila+1,il_Col_tot_Semana,ll_TotalC)

RETURN
end subroutine

public subroutine wf_totalcalibre (long fila);//Por defecto siempre viene la fila de los porcentajes
Integer	li_Columna
Decimal{2}	ld_Porcen, ld_TotalP
Long			ll_Cajas, ll_TotalC

dw_1.AcceptText()

FOR li_Columna = il_Col_tot_Semana + 1 to il_Col_tot_Calibre - 1
	ld_Porcen	=	dw_1.GetItemDecimal(fila,li_Columna)
	IF Isnull(ld_Porcen) THEN 
		ld_Porcen = 0 
		dw_1.SetItem(fila,li_Columna,ld_Porcen)
	END IF
	ld_TotalP		=	ld_TotalP + ld_Porcen
	ll_Cajas		=	dw_1.GetItemDecimal(fila + 1,li_Columna)
	IF Isnull(ll_Cajas) THEN 
		ll_Cajas = 0
		dw_1.SetItem(fila + 1,li_Columna,ll_Cajas)
	END IF		
	ll_TotalC		=	ll_TotalC + ll_Cajas
NEXT

dw_1.SetItem(fila,il_Col_tot_Calibre,ld_TotalP)
dw_1.SetItem(fila+1,il_Col_tot_Calibre,ll_TotalC)

RETURN
end subroutine

public subroutine wf_recalcula (integer fila);//Por defecto siempre viene la fila de los porcentajes
Integer	li_Columna
Decimal{2}	ld_Porcen, ld_TotalP
Long			ll_Cajas, ll_TotalC, ll_TCajas

dw_1.AcceptText()

ll_TCajas	=	dw_1.Object.valor_condicion[fila+1]
		
FOR li_Columna = 6 to il_Col_tot_Semana - 1
	ld_Porcen	=	dw_1.GetItemDecimal(fila,li_Columna)
	IF Isnull(ld_Porcen) THEN 
		ld_Porcen = 0 
		dw_1.SetItem(fila,li_Columna,ld_Porcen)
	END IF
	ld_TotalP		=	ld_TotalP + ld_Porcen
	
	ll_Cajas	=	Round(ll_TCajas*ld_Porcen/100,0)
	IF Isnull(ll_Cajas) THEN 
		ll_Cajas = 0
	END IF
	dw_1.SetItem(fila + 1,li_Columna,ll_Cajas)
			
	ll_TotalC		=	ll_TotalC + ll_Cajas
NEXT

dw_1.SetItem(fila,il_Col_tot_Semana,ld_TotalP)
dw_1.SetItem(fila+1,il_Col_tot_Semana,ll_TotalC)

ll_TotalC = 0
ld_TotalP = 0

FOR li_Columna = il_Col_tot_Semana + 1 to il_Col_tot_Calibre - 1
	ld_Porcen	=	dw_1.GetItemDecimal(fila,li_Columna)
	IF Isnull(ld_Porcen) THEN 
		ld_Porcen = 0 
		dw_1.SetItem(fila,li_Columna,ld_Porcen)
	END IF
	ld_TotalP		=	ld_TotalP + ld_Porcen
	
	ll_Cajas	=	Round(ll_TCajas*ld_Porcen/100,0)
	IF Isnull(ll_Cajas) THEN 
		ll_Cajas = 0
	END IF
	dw_1.SetItem(fila + 1,li_Columna,ll_Cajas)
			
	ll_TotalC		=	ll_TotalC + ll_Cajas
NEXT

dw_1.SetItem(fila,il_Col_tot_Calibre,ld_TotalP)
dw_1.SetItem(fila+1,il_Col_tot_Calibre,ll_TotalC)

RETURN
end subroutine

on w_plan_pronostico_cosecha_agregado.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
this.dw_5=create dw_5
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_5
this.Control[iCurrent+4]=this.dw_4
end on

on w_plan_pronostico_cosecha_agregado.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.dw_5)
destroy(this.dw_4)
end on

event open;Integer li_null
isnull(li_null)

x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 2500
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

istr_mant.argumento[6] = ""

istr_busq	=	Message.PowerObjectParm

iuo_especie				=	Create uo_Especie
iuo_variedad			=	Create uo_Variedades
iuo_productor	      	=	Create uo_Productores
iuo_predio				=	Create uo_Prodpredio
iuo_Cuartel				=	Create uo_ProdCuarteles
iuo_semana				=	Create uo_Nrosemana

ids_Calibre = Create DataStore
ids_Calibre.DataObject	=	'dw_cons_calibres_identificador'
ids_Calibre.SetTransObject(sqlca)

//Productor//
dw_2.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
IF idwc_productor.Retrieve(-1) = 0 THEN
	idwc_Productor.InsertRow(0)
END IF

//Predio//
dw_2.GetChild("prpr_codigo", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.InsertRow(0)

//Cuartel//
dw_2.GetChild("prcc_codigo", idwc_Cuartel)
idwc_Cuartel.SetTransObject(sqlca)
idwc_Cuartel.InsertRow(0)

// Especie//
dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	idwc_especie.InsertRow(0)
END IF

//Variedad//
dw_2.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)
idwc_variedad.InsertRow(0)

dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

dw_2.InsertRow(0)

dw_2.object.prod_codigo[1] = Long(istr_Busq.Argum[6])
dw_2.object.prpr_codigo[1] = Integer(istr_Busq.Argum[7])
dw_2.object.prcc_codigo[1] = Integer(istr_Busq.Argum[8])
dw_2.object.todoscuartel[1] = Integer(istr_Busq.Argum[9])
dw_2.object.espe_codigo[1] = Integer(istr_Busq.Argum[3])
dw_2.object.vari_codigo[1] = Integer(istr_Busq.Argum[4])
dw_2.object.todosvarie[1] = Integer(istr_Busq.Argum[5])
dw_2.object.dfce_semini[1] = Integer(istr_Busq.Argum[1])
dw_2.object.dfce_lunini[1] = Datetime(Date(istr_Busq.Argum[2]))

iuo_Productor.Existe(Long(istr_busq.argum[6]), True, SQLCA) 
iuo_Predio.Existe(Long(istr_Busq.Argum[7]), iuo_Productor.Codigo, True, SQLCA)
iuo_Especie.Existe(Integer(istr_Busq.Argum[3]),False,sqlca)
IF Integer(istr_busq.Argum[5]) = 0 THEN
	iuo_variedad.existe(Integer(istr_Busq.Argum[3]),Integer(istr_Busq.Argum[4]),False,Sqlca)
END IF

If idwc_predio.Retrieve(iuo_productor.Codigo) = 0 Then
	dw_2.setitem( 1,'prpr_codigo', li_null)
Else
	idwc_predio.InsertRow(0)
End If

If idwc_variedad.Retrieve(-1, -1, integer(istr_Busq.Argum[3])) = 0 Then
	dw_2.Object.vari_codigo[1] = li_Null
Else
	idwc_variedad.SetTransObject(SqlCa)
	idwc_variedad.InsertRow(0)
End If
end event

event ue_recuperadatos;Long	 		ll_Fila, ll_Fila_d,ll_fila_c, ll_fila_s, ll_fila_q, respuesta
String			ls_Columna
Decimal{2}	ld_valor,ld_valord
Integer		li_CodCondAnt,li_CodCond,li_Tipo_RegAnt,li_Tipo_Reg

DO
	If dw_2.Update(True,False) = 1 Then 
		dw_2.ResetUpdate()
		ids_Calibre.Retrieve(dw_2.Object.dfce_identi[1])
		wf_crea_datawindow()
		
		dw_3.SetTransObject(sqlca)
		dw_4.SetTransObject(sqlca)
		dw_5.SetTransObject(sqlca)
		
		ll_fila_q=dw_3.Retrieve(dw_2.Object.dfce_identi[1])
		If  ll_fila_q > 0 Then
			If dw_2.Object.dfce_tcajas[1]	<> dw_3.Object.total_cajas[1] Then
				dw_2.Object.dfce_tcajas[1] =	dw_3.Object.total_cajas[1]
				dw_2.Update(True,False)
				dw_2.ResetUpdate()
			End If
		End If
		ll_Fila_c	=	dw_4.Retrieve(dw_2.Object.dfce_identi[1])
		ll_fila_s	=	dw_5.Retrieve(dw_2.Object.dfce_identi[1])
		
		//Llenado de Datawindow Dinámica según semanas
		FOR ll_Fila = 1 to ll_Fila_s
			li_CodCond	=	dw_5.Object.cacm_codigo[ll_Fila]
			li_Tipo_Reg	=	dw_5.Object.dfcc_tipreg[ll_Fila]
			If li_CodCond <> li_CodCondAnt Or li_Tipo_Reg <> li_Tipo_RegAnt Then
				ll_Fila_d = dw_1.InsertRow(0)
				dw_1.Object.IdentIficador[ll_Fila_d]		=	dw_2.Object.dfce_identi[1]
				dw_1.Object.Cod_Condicion[ll_Fila_d]	=	li_CodCond
				dw_1.Object.Condicion[ll_Fila_d]			=	dw_5.Object.calm_nombre[ll_Fila]
				dw_1.Object.Tipo_Registro[ll_Fila_d]		=	li_Tipo_Reg
				If Isnull(dw_5.Object.dfcc_valord[ll_Fila]) Then 
					ld_valord = 0
				Else
					ld_valord = dw_5.Object.dfcc_valord[ll_Fila]
				End If
				
				dw_1.Object.valor_Condicion[ll_Fila_d]	=	ld_valord
				li_CodCondAnt	=	li_CodCond
				li_Tipo_RegAnt	=	li_Tipo_Reg
			End If
			
			ls_Columna = 'Valor_Sem'+String(dw_5.Object.dfcc_semana[ll_Fila],'00')
			If Isnull(dw_5.Object.dfcc_valors[ll_Fila]) Then
				ld_valor = 0
			Else
				ld_valor	=	dw_5.Object.dfcc_valors[ll_Fila]
			End If
			dw_1.SetItem(ll_Fila_d,ls_Columna,ld_valor)
		NEXT
		
		//Llenado de Datawindow Dinámica según Calibres
		FOR ll_Fila = 1 to ll_Fila_c
			li_CodCond	=	dw_4.Object.cacm_codigo[ll_Fila]
			li_Tipo_Reg	=	dw_4.Object.dfcc_tipreg[ll_Fila]
			
			ll_Fila_d = dw_1.Find('Cod_Condicion = '+String(li_CodCond)+' And Tipo_Registro = '+String(li_Tipo_Reg),1,dw_1.RowCount())
			
			If ll_Fila_d = 0 Then
				ll_Fila_d = dw_1.InsertRow(0)
				dw_1.Object.IdentIficador[ll_Fila_d]	=	dw_2.Object.dfce_identi[1]
				dw_1.Object.Cod_Condicion[ll_Fila_d]	=	li_CodCond
				//dw_1.Object.Condicion[ll_Fila_d]			=	dw_4.Object.calm_nombre[ll_Fila]
				dw_1.Object.Tipo_Registro[ll_Fila_d]		=	li_Tipo_Reg
				
				If Isnull(dw_4.Object.dfcc_valord[ll_Fila]) Then 
					ld_valord = 0
				Else
					ld_valord = dw_4.Object.dfcc_valord[ll_Fila]
				End If
				
				dw_1.Object.valor_Condicion[ll_Fila_d]	=	ld_valord
				li_CodCondAnt	=	li_CodCond
				li_Tipo_RegAnt	=	li_Tipo_Reg
			End If
			ls_Columna = 'Valor_Cal'+dw_4.Object.vaca_calibr[ll_Fila]
			If Isnull(dw_4.Object.dfcc_valorc[ll_Fila]) Then
				ld_valor	=	0
			Else
				ld_valor	=	dw_4.Object.dfcc_valorc[ll_Fila]
			End If
			dw_1.SetItem(ll_Fila_d,ls_Columna,ld_valor)
		NEXT
		
		FOR ll_Fila_d = 1 TO dw_1.RowCount()
			li_Tipo_Reg	=	dw_1.Object.Tipo_Registro[ll_Fila_d]
			If li_Tipo_Reg = 1 Then
				wf_TotalSemana(ll_Fila_d)
				wf_TotalCalibre(ll_Fila_d)
			End If
		NEXT
		
		il_fila				= 1
		
		dw_1.SetRow(1)
		dw_1.SetFocus()
		
		pb_grabar.Enabled	=	Not istr_Mant.Solo_Consulta
		pb_eliminar.Enabled	=	Not istr_Mant.Solo_Consulta
		pb_imprimir.Enabled	=	True
		Habilitaenca(FALSE)
	End If
	
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_antesguardar;Long		ll_Fila, ll_Fila_s, ll_Fila_c
Integer	li_Columna
String		ls_Columna
Decimal{2}	ld_valor

If IsNull(dw_2.Object.dfce_semter[1]) OR dw_2.Object.dfce_semter[1]	= 0 Then
	MessageBox("Atención","Debe especIficar la semana de termino a Distribuir.")
	dw_2.SetColumn("dfce_semter")
	Message.DoubleParm = -1
	Return
End If

//Elimina datos capturados y los vuelve a registrar
Datastore lds_Cuarteles, lds_Calibres, lds_Semana

lds_Cuarteles	=	Create DataStore
lds_Calibres		=	Create DataStore
lds_Semana		=	Create DataStore

lds_Cuarteles.DataObject	=	dw_3.DataObject
lds_Calibres.DataObject 		=	dw_4.DataObject
lds_Semana.DataObject		=	dw_5.DataObject

lds_Cuarteles.SetTransObject(SQLCA)
lds_Calibres.SetTransObject(SQLCA)
lds_Semana.SetTransObject(SQLCA)

IF IsNull(dw_3.Object.dfcc_idcuar[1]) THEN
	dw_3.RowsMove(1,dw_3.RowCount(),Primary!,lds_Cuarteles,1,Primary!)
	dw_3.Reset()

	FOR ll_Fila = 1 TO lds_Cuarteles.RowCount()
		ll_Fila_c = dw_3.InsertRow(0)
		dw_3.Object.dfce_identi[ll_Fila_c]	=	dw_2.Object.dfce_identi[1]
		dw_3.Object.prod_codigo[ll_Fila_c]	=	lds_Cuarteles.Object.prod_codigo[ll_Fila]
		dw_3.Object.prpr_codigo[ll_Fila_c]	=	lds_Cuarteles.Object.prpr_codigo[ll_Fila]
		dw_3.Object.prcc_codigo[ll_Fila_c]	=	lds_Cuarteles.Object.prcc_codigo[ll_Fila]
		dw_3.Object.prcc_nombre[ll_Fila_c]	=	lds_Cuarteles.Object.prcc_nombre[ll_Fila]
		dw_3.Object.espe_codigo[ll_Fila_c]	=	lds_Cuarteles.Object.espe_codigo[ll_Fila]
		dw_3.Object.vari_codigo[ll_Fila_c]	=	lds_Cuarteles.Object.vari_codigo[ll_Fila]
		dw_3.Object.vari_nombre[ll_Fila_c]	=	lds_Cuarteles.Object.vari_nombre[ll_Fila]
		ld_valor	=	lds_Cuarteles.Object.dfcc_tcajas[ll_Fila]
		IF Isnull(ld_valor) THEN ld_Valor = 0
		dw_3.Object.dfcc_tcajas[ll_Fila_c]	=	ld_Valor
	NEXT
END IF

IF IsNull(dw_4.Object.dfcc_idcoca[1]) THEN
	dw_4.RowsMove(1,dw_4.RowCount(),Primary!,lds_Calibres,1,Delete!)
	dw_4.Reset()
ELSE
	dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)	
END IF

IF IsNull(dw_5.Object.dfcc_idcose[1]) THEN
	dw_5.RowsMove(1,dw_5.RowCount(),Primary!,lds_Semana,1,Delete!)
	dw_5.Reset()
ELSE
	dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)	
END IF

FOR ll_Fila= 1 to dw_1.RowCount()
	//Alimenta Condición y Semanas
	FOR li_Columna =	6 to il_Col_tot_Semana - 1
		ll_Fila_s = dw_5.InsertRow(0)
		dw_5.Object.dfce_identi[ll_Fila_s]		=	dw_2.Object.dfce_identi[1]
		dw_5.Object.dfcc_tipreg[ll_Fila_s]		=	dw_1.Object.tipo_registro[ll_Fila]
		dw_5.Object.cacm_codigo[ll_Fila_s]	=	dw_1.Object.Cod_Condicion[ll_Fila]
		dw_5.Object.dfcc_valord[ll_Fila_s]		=	dw_1.Object.Valor_Condicion[ll_Fila]
		ls_Columna	=	is_col[li_Columna - 5]
		dw_5.Object.dfcc_semana[ll_Fila_s]	=	Integer(Mid(ls_Columna,10,2))
		ld_valor	=	dw_1.GetItemDecimal(ll_Fila,ls_Columna)
		If IsNull(ld_valor) Then ld_Valor 		= 0
		dw_5.Object.dfcc_valors[ll_Fila_s] 	= 	ld_Valor
	NEXT
	//Alimenta Condición y Calibres
	FOR li_Columna =	il_Col_tot_Semana + 1 to il_Col_tot_Calibre - 1
		ll_Fila_c = dw_4.InsertRow(0)
		dw_4.Object.dfce_identi[ll_Fila_c]		=	dw_2.Object.dfce_identi[1]
		dw_4.Object.dfcc_tipreg[ll_Fila_c]		=	dw_1.Object.tipo_registro[ll_Fila]
		dw_4.Object.cacm_codigo[ll_Fila_c]	=	dw_1.Object.Cod_Condicion[ll_Fila]
		dw_4.Object.dfcc_valord[ll_Fila_c]		=	dw_1.Object.Valor_Condicion[ll_Fila]
		ls_Columna	=	is_col[li_Columna - 6]
		dw_4.Object.vaca_calibr[ll_Fila_c]		=	Mid(ls_Columna,10,4)
		ld_valor	=	dw_1.GetItemDecimal(ll_Fila,ls_Columna)
		If IsNull(ld_valor) Then ld_Valor 		= 0
		dw_4.Object.dfcc_valorc[ll_Fila_c] 	= 	ld_Valor
	NEXT
NEXT
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	wf_traspasaDistribucion()
	
	w_mant_mues_flujocosechamulsemanal_packing.wf_CargaDetalle()	
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

Close(THis)
end event

event ue_borrar;Long	ll_Fila	

IF dw_2.Object.rdte_estado[1] = 1 THEN 
	MessageBox(This.Title,"No se puede borrar actual registro,  ya que estado es Definitivo.")
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Pronóstico", Question!, YesNo!) = 1 THEN
	FOR ll_Fila = 1 to dw_1.RowCount()
		dw_1.Object.prcc_cajcuar[ll_Fila] = 0
	NEXT	
END IF
end event

event resize;Integer		li_posic_x, li_posic_y, li_visible, &
				li_Ancho = 300, li_Alto = 245, li_Siguiente = 255, maximo

dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)

maximo	= dw_1.width

If dw_2.width > maximo Then maximo = dw_2.width

li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	st_encabe.y

pb_lectura.x				= li_posic_x
pb_lectura.y				= li_posic_y
pb_lectura.width		= li_Ancho
pb_lectura.height		= li_Alto
li_posic_y 				+= li_Siguiente

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					= 64 + dw_2.Height

dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41


If pb_nuevo.Visible Then
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width		=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_visible 				++
	li_posic_y 				+= li_Siguiente
End If

If pb_insertar.Visible Then
	pb_insertar.x			=	li_posic_x
	pb_insertar.y			=	li_posic_y
	pb_insertar.width		=	li_Ancho
	pb_insertar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_eliminar.Visible Then
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_grabar.Visible Then
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_imprimir.Visible Then
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

pb_Salir.x				=	li_posic_x
pb_Salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_Salir.width			=	li_Ancho
pb_Salir.height			=	li_Alto

dw_3.x	=	dw_2.x + 755
dw_3.y	=	dw_2.y + dw_2.Height - 20
end event

type st_encabe from w_mant_directo`st_encabe within w_plan_pronostico_cosecha_agregado
boolean visible = false
integer x = 55
integer y = 20
integer width = 2839
integer height = 412
integer taborder = 10
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_plan_pronostico_cosecha_agregado
boolean visible = false
integer x = 3872
integer y = 428
integer taborder = 120
end type

event pb_nuevo::clicked;call super::clicked;dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1,"pate_tempor",gstr_tempo.Temporada)

istr_mant.argumento[1]	=	'0'
istr_mant.argumento[2]	=	'0'
istr_mant.argumento[3]	=	'0'
istr_mant.argumento[4]	=	'0'

SetNull(iuo_Productor.Codigo)
SetNull(iuo_Predio.Codigo)

pb_grabar.enabled		=	False
pb_imprimir.enabled		=	False

il_Fila					=	0

Habilitaenca(True)

If gstr_agro.codigoagronomo<>0 and isnull(gstr_agro.codigoagronomo)=False Then
	dw_2.Object.agro_codigo[1] 	=	gstr_agro.codigoagronomo
	istr_mant.argumento[5] 			=	string(gstr_agro.codigoagronomo)
	If gstr_agro.administrador=0 or isnull(gstr_agro.codigoagronomo) Then
		dw_2.Object.agro_codigo.Protect = 1
	End If
End If
end event

type pb_lectura from w_mant_directo`pb_lectura within w_plan_pronostico_cosecha_agregado
integer x = 3872
integer taborder = 30
boolean default = true
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_plan_pronostico_cosecha_agregado
boolean visible = false
integer x = 3867
integer y = 1688
integer taborder = 0
string picturename = "\Desarrollo 17\Imagenes\Botones\Basurero.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Basurero-bn.png"
end type

type pb_insertar from w_mant_directo`pb_insertar within w_plan_pronostico_cosecha_agregado
boolean visible = false
integer x = 3881
integer y = 1344
integer taborder = 0
end type

type pb_salir from w_mant_directo`pb_salir within w_plan_pronostico_cosecha_agregado
integer x = 3872
integer y = 1536
integer taborder = 180
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_plan_pronostico_cosecha_agregado
boolean visible = false
integer x = 3872
integer y = 1028
integer taborder = 160
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type pb_grabar from w_mant_directo`pb_grabar within w_plan_pronostico_cosecha_agregado
integer x = 3872
integer y = 716
integer taborder = 140
end type

type dw_1 from w_mant_directo`dw_1 within w_plan_pronostico_cosecha_agregado
integer x = 64
integer y = 452
integer width = 3470
integer height = 796
integer taborder = 40
boolean hscrollbar = true
boolean border = true
boolean hsplitscroll = true
end type

event dw_1::itemchanged;call super::itemchanged;String		ls_Columna
Dec{2}	ld_Valor, ld_Porcen	
Integer	li_Tipo_Registro
Long		ll_TCajas, ll_Cajas

ls_Columna	=	dwo.Name

Choose Case UPPER(Mid(ls_Columna,1,9))
	Case "VALOR_CON"
		li_Tipo_Registro	=	This.Object.tipo_registro[row]
		ll_TCajas				=	dw_2.Object.dfce_tcajas[1]
		
		If li_Tipo_Registro = 2 Then
			//Ingreso de Cajas
			If ll_TCajas = 0 Then ll_TCajas = Dec(Data)
			
			ld_Porcen = Round(Dec(Data)*100/ll_TCajas,2)
			This.Object.valor_condicion[row - 1] = ld_Porcen
			wf_recalcula(row - 1)
		Else
			//Ingreso de Porcentaje
			ll_Cajas	=	Round(ll_TCajas*Dec(data)/100,0)
			This.Object.valor_condicion[row + 1] = ll_Cajas
			wf_recalcula(row)
		End If
		This.GroupCalc()
		dw_2.Object.dfce_tcajas[1] = This.Object.Compute_14[Row]
		
	Case "VALOR_SEM"
		li_Tipo_Registro	=	This.Object.tipo_registro[row]
		If li_Tipo_Registro = 2 Then
			//Ingreso de Cajas
			ll_TCajas	=	This.Object.valor_condicion[row]
			If ll_TCajas = 0 Then ll_TCajas = Dec(Data)
			
			ld_Porcen = Round(Dec(Data)*100/ll_TCajas,2)
			This.SetItem(row - 1,ls_Columna, ld_Porcen)
			wf_TotalSemana(row - 1)
		Else
			ll_TCajas	=	This.Object.valor_condicion[row + 1]
			ll_Cajas	=	Round(ll_TCajas*Dec(data)/100,0)
			This.SetItem(row + 1,ls_Columna, ll_Cajas)
			wf_TotalSemana(row)
		End If

	Case 'VALOR_CAL'
		li_Tipo_Registro	=	This.Object.tipo_registro[row]
		If li_Tipo_Registro = 2 Then
			//Ingreso de Cajas
			ll_TCajas	=	This.Object.valor_condicion[row]
			If ll_TCajas = 0 Then ll_TCajas = Dec(Data)
			
			ld_Porcen = Round(Dec(Data)*100/ll_TCajas,2)
			This.SetItem(row - 1,ls_Columna, ld_Porcen)
			wf_TotalCalibre(row - 1)
		Else
			ll_TCajas	=	This.Object.valor_condicion[row + 1]
			ll_Cajas	=	Round(ll_TCajas*Dec(data)/100,0)
			This.SetItem(row + 1,ls_Columna, ll_Cajas)
			wf_TotalCalibre(row)
		End If
End Choose 
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE Key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type dw_2 from datawindow within w_plan_pronostico_cosecha_agregado
integer x = 59
integer y = 20
integer width = 2999
integer height = 412
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_pron_distfrujocosenca"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Semana, li_SemNulo[], li_Sem, li_ultsemana
String		ls_columna, ls_Null
Date		ld_Lunes

SetNull(ls_Null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "dfce_semter"	
		If iuo_Semana.Semana(Integer(Data), gstr_tempo.temporada, iuo_Especie.Codigo) Then
			li_ultsemana	=	F_NroSemanaAno(iuo_Semana.Lunes)
			If Integer(data) = 53 Then
				If 	li_ultsemana = 52 Then
					This.SetItem(row, ls_Columna, String(ls_Null))
					MessageBox('Alerta', 'Este año no posee semana 53.', Exclamation!, OK!)
					Return 1
				End If
			End If
			
			IF Datetime(iuo_Semana.Lunes) < This.Object.dfce_lunini[1] THEN
				This.SetItem(row, ls_Columna, String(ls_Null))
				MessageBox('Alerta', 'Semana de Término debe ser posterior a Inicio.', Exclamation!, OK!)
				Return 1
			End If
			
			dw_2.Object.dfce_lunter[1] = Datetime(iuo_Semana.Lunes)
			
			/*
			Llena Vector con Semanas desde Semana de Inicio
			*/
			ii_Semanas = li_SemNulo
			ld_Lunes	=	Date(This.Object.dfce_lunini[1])
			li_Semana	=	This.Object.dfce_semini[1]
			
			DO WHILE ld_Lunes <= iuo_Semana.Lunes
				li_Sem++
				ii_Semanas[li_Sem] = li_Semana
				
				ld_Lunes	=	RelativeDate(ld_Lunes,7)
				li_semana	++
				
				li_ultsemana	=	F_NroSemanaAno(ld_Lunes)
				If li_Semana = 53 Then
					IF li_ultsemana = 52 THEN
						li_semana = 1
					END IF
				END IF
			LOOP
			HabilitaEnca(False)
		End If	
End Choose		
end event

event itemerror;Return 1
end event

event buttonclicked;String		ls_columna
Boolean	lb_Visibilidad

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	CASE 'b_cajxcuartel'
		lb_Visibilidad = dw_3.Visible
		
		IF dw_3.RowCount()>0 AND lb_Visibilidad THEN
			dw_3.AcceptText()
			dw_2.Object.dfce_tcajas[1]	=	dw_3.Object.total_cajas[1]
			//Llama a funcion Redistribuye total de Cajas
			
		END IF
		dw_3.Visible = Not lb_Visibilidad
		
END CHOOSE
end event

type dw_3 from datawindow within w_plan_pronostico_cosecha_agregado
boolean visible = false
integer x = 814
integer y = 396
integer width = 1486
integer height = 540
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_pron_distfrujocoscuartel"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event losefocus;This.AcceptText()
end event

type dw_5 from datawindow within w_plan_pronostico_cosecha_agregado
boolean visible = false
integer y = 1292
integer width = 686
integer height = 400
integer taborder = 190
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_pron_distfrujocoscondsemana"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_plan_pronostico_cosecha_agregado
boolean visible = false
integer x = 768
integer y = 1296
integer width = 686
integer height = 400
integer taborder = 30
string title = "none"
string dataobject = "dw_mues_pron_distfrujocoscondcalibre"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

